000100990322     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200981019     H DEBUG DECEDIT('0,') DATEDIT(*DMY.)
000300000000     H*--------------------------------------------------------------*
000400070529     Ftrmz74d   CF   E             WORKSTN
000500070605     Faitra04l  if   e           k disk
000600090625     Faitrs01l  if   e           k disk
000700050623     Fazorg01l  if   e           k disk    rename(azorg:azorg1)
000800050622     Fazorg02l  if   e           k disk
000900050802     Fwaitra0f  uf a e           k disk    usropn
001000121031     Fwaitra2f  uf a e           k disk    usropn
001100050801     ftabel00f  if   e           k disk
001200050805     Fqsysprt   O    F  132        PRINTER prtctl(prtds)
001300020304     dkpjba          e ds
001400151116     d Tibs36Ds      e ds
001500070529     d Tibs02Ds      e ds
001600151116     D trmz70s1ds    E DS
001700121105     d datu          e ds
001800020404     d prtds           ds
001900020404     d   spab                         3  0
002000020404     d   spaa                         3  0
002100020404     d   skab                         3  0
002200020404     d   skaa                         3  0
002300020404     d   line                         3  0
002400050623     d totmzfil        s              5  0
002500050623     d totmzarea       s              5  0
002600050623     d totmzdis        s              5  0
002700050623     d totmzaz         s              5  0
002800050623     d primo           s              1
002900050801     d exaread         s             14
003000051121     d exaread20       s             20
003100051121     d exdistd20       s             20
003200050623     d excar           s                   like(orgcar)
003300050802     d warea           s                   like(orgcar)
003400050623     d kfil            s                   like(orgfil)
003500050803     d exfil           s                   like(orgfil)
003600050623     d exfl3           s                   like(orgfl3)
003700050802     d wdist           s                   like(orgfl3)
003800070608     d coman           s             99
003900050802     d luncmd          s             15  5
004000070605     d dataiso         s               d   datfmt(*iso)
004100070605     d dataeur         s               d   datfmt(*eur)
004200070605     D WLBDA8          DS
004300070605     D  G02DAT                 1      8  0
004400070605     D  G02INV                 9     16  0
004500070605     D  G02ERR                17     17
004600070605     D  G02TGI                18     22  0
004700080502      *
004800070608     d file            s             10
004900070608     D cmd1            S              1    DIM(99) CTDATA PERRCD(99)
005000070608     D cmd2            S              1    DIM(99) CTDATA PERRCD(99)
005100070608     D cmd3            S              1    DIM(99) CTDATA PERRCD(99)
005200070608     D cmd4            S              1    DIM(99) CTDATA PERRCD(99)
005300070608     D cmd5            S              1    DIM(99) CTDATA PERRCD(99)
005400121105     d file2           s             10
005500121002     D cmd7            S              1    DIM(99) CTDATA PERRCD(99)
005600121002     D cmd8            S              1    DIM(99) CTDATA PERRCD(99)
005700121002     D cmd9            S              1    DIM(99) CTDATA PERRCD(99)
005800121002     D cmd11           S              1    DIM(99) CTDATA PERRCD(99)
005900000000     C*---------------------------------------------------------------*
006000020304      *
006100031105     c                   do        *hival
006200031105     c                   exfmt     video1
006300031105     c                   clear                   $msg
006400091217     c   kc              leave
006500031105     c                   exsr      controv1
006600031105     c   96              iter
006700050623     c                   if        *inkf
006800091216      *
006900091111     c                   if        vstampa = 'S'
007000070531     c                   except    legenda
007100070601     c                   z-add     99            line
007200091216      *
007300091111     c                   endif
007400091216      *
007500050805      * verifico indicatore per salto pagina forzato
007600050805     c     saltp         comp      'S'                                    66
007700050802      *clear file
007800050802     c                   movea     cmd1          coman
007900050802     c                   exsr      comandi
008000121002     c                   movea     cmd7          coman
008100121002     c                   exsr      comandib
008200050802
008300050623     c                   exsr      elabora
008400050805     c                   leave
008500050623     c                   endif
008600050805     c                   enddo
008700160115      **
008800050802      * se richiesta stampa per area o distretto la eseguo dopo aver
008900050802      * completato il W.F con i dati selezionati
009000070531     c*                  if        lisarea = 'S' and *in80 or
009100070531     c*                            lisdist = 'S' and *in81
009200091111      * se si vuole la stampa
009300100114     c                   if        vstampa = 'S'
009400050802     c                   exsr      waitra
009500091111     c                   endif
009600160115      ***
009700070531     c*                  endif
009800160115      ***
009900160115      * COMUNQUE prova a cancellare il file se esisteva sulla WRKQUERY
010000070608     c                   movea     cmd4          coman
010100070608     c                   eval      %subst(coman:22:10) = file
010200070608     c                   exsr      comandi2
010300160115      *
010400160115      * e LO CREA NUOVAMENTE
010500070608     c                   movea     cmd5          coman
010600070608     c                   eval      %subst(coman:78:10) = file
010700070608     c                   exsr      comandi2
010800160115      *
010900121003      ** se cambiato il tracciato del file lo deve segnalare
011000121003     c                   if        not *inKC
011100121003     c   34              eval      w1nomf = file
011200121003     c   34              exfmt     w1Error
011300121003     c                   end
011400121003      **
011500121002      *SECONDO FILE DI WORK
011600160115      * COMUNQUE prova a cancellare il file se esisteva sulla WRKQUERY
011700160115     c                   movea     cmd4          coman
011800121105     c                   eval      %subst(coman:22:10) = file2
011900121002     c                   exsr      comandi2
012000160115      *
012100160115      * e LO CREA NUOVAMENTE
012200121002     c                   movea     cmd11         coman
012300121105     c                   eval      %subst(coman:78:10) = file2
012400121002     c                   exsr      comandi2
012500121002      *
012600091217      ** se cambiato il tracciato del file lo deve segnalare
012700091217     c                   if        not *inKC
012800091217     c   34              eval      w1nomf = file
012900091217     c   34              exfmt     w1Error
013000091217     c                   end
013100091217      **
013200050802      *elimina file in qtemp x la filiale
013300051017     c                   movea     cmd3          coman
013400051017     c                   exsr      comandi
013500121002     c                   movea     cmd9          coman
013600121002     c                   exsr      comandib
013700091217      *
013800091217     c     fine          tag
013900020308     c                   seton                                        lr
014000050622     c*-------------------------------------------------
014100050622     c     elabora       begsr
014200050622     c*-------------------------------------------------
014300050622     c                   clear                   v1dfil
014400070529 1   c                   if        v1cfil > 0 and lisarea = *blank and
014500050802     c                             lisdist = *blank
014600050623     c                   move      v1cfil        kfil
014700050622     c                   exsr      aitra
014800070529 x1  c                   else
014900050622     c     *loval        setll     azorg02l
015000070529 2   c                   do        *hival
015100050622     c                   read      azorg02l
015200070529 3   c                   if        %eof(azorg02l) or orgfl3 > '5'
015300050622     c                   leave
015400070529 e3  c                   endif
015500051122      * SEDE solo le filiali di un'area se selezionata a video
015600051122     c                   move      orgcar        orgcara           3
015700070529 3   c                   if        orgcara <> varea and varea <> *blank
015800051122     c                   iter
015900070529 e3  c                   endif
016000051122      * SEDE solo le filiali di un distretto se selezionato a video
016100051122     c                   if        orgfl3 <> vdist and vdist <> *blank
016200051122     c                   iter
016300051122     c                   endif
016400050623      * solo le filiali
016500050805     c                   if        orgfag <>'F' or %subst(orgde8:1:1) = *blank
016600050623     c                   iter
016700050623     c                   endif
016800050802      * scarto se profilo di area o di distretto e voglio sapere solo i miei
016900070529     c                   if        lisarea = 'S' and *in80
017000070529     c                   if        orgcar <> warea
017100050802     c                   iter
017200050802     c                   endif
017300050802     c                   endif
017400070619     c                   if        lisdist = 'S' and *in81
017500070619     c                   if        orgfl3 <> wdist
017600050802     c                   iter
017700050802     c                   endif
017800050802     c                   endif
017900070531     c                   move      orgfil        kfil
018000050622     c                   exsr      aitra
018100070619 e2  c                   enddo
018200070619 e1  c                   endif
018300050622
018400050622     c                   endsr
018500990113     c*-------------------------------------------------
018600030630     c     aitra         begsr
018700990113     c*-------------------------------------------------
018800070605     c     kfil          setll     aitra04l
018900981029     c                   do        *hival
019000070605     c     kfil          reade     aitra04l                               98
019100031105     c   98              leave
019200090625     c     tranrc        chain     aitrs01l
019300090625     c                   if        %found(aitrs01l)
019400090625     c                   movel     trsiva        traivas
019500090625     c                   else
019600090625     c                   clear                   traivas
019700090625     c                   endif
019800090625      *
019900070530      * esclude annullati e corrispondenti
020000091006     c                   if        traann <> *blank
020100040212     c                   iter
020200040212     c                   endif
020300081204      * esclude se selezionati autisti di città o aff/def
020400081204     c                   if        vtpaut <> *blank
020500081204     c                   if        vtpaut = 'C' and trakau = 0
020600081204     c                   iter
020700081204     c                   endif
020800081204     c                   if        vtpaut = 'A' and trabpt = 0
020900081204     c                   iter
021000081204     c                   endif
021100081204     c                   endif
021200070605      * esclude disaccreditati se no turnover
021300091111     c                   if        Vtradfi = 0 and tradfi > 0
021400091111     c                   iter
021500091111     c                   endif
021600070530      * analizza filtri a video
021700170221      * Senza automezzo
021800170221      * ex Autista di riserva
021900070530     c                   if        vtraris <> *blank
022000070530     c                   if        vtraris = 'S' and traris = *blank or
022100070530     c                             vtraris = 'N' and traris <> *blank
022200070530     c                   iter
022300070530     c                   endif
022400070531     c                   endif
022500091006      * Autista a perdere
022600091006     c                   if        vtracor <> *blank
022700091006     c                   if        vtracor = 'S' and tracor = *blank or
022800091006     c                             vtracor = 'N' and tracor <> *blank
022900091006     c                   iter
023000091006     c                   endif
023100091006     c                   endif
023200070530      * Mezzo di scorta
023300070530     c                   if        vtrasco <> *blank
023400070530     c                   if        vtrasco = 'S' and trasco = *blank or
023500070530     c                             vtrasco = 'N' and trasco <> *blank
023600070530     c                   iter
023700070530     c                   endif
023800120914     c                   endif
023900120914      * Mezzi CENTRO ST/LOC.TURISTICHE
024000120914     c                   if        vtraloci <> *blank
024100120914     c                   if        vtraloci = 'S' and traloci = *blank
024200120914     c                             or
024300120914     c                             vtraloci = 'N' and traloci <> *blank
024400120914     c                   iter
024500120914     c                   endif
024600120914     c                   endif
024700070530      * colore automezzo
024800070530     c                   if        vtratmp <> *blank
024900070530     c                   if        vtratmp = 'S' and tratmp <> 'S'  or
025000070530     c                             vtratmp = 'N' and tratmp = 'S'
025100070530     c                   iter
025200070530     c                   endif
025300070531     c                   endif
025400070606      * turnover
025500070606     c                   if        vtratur <> *blank
025600091111     c                   if        tradfi < vtradfig or
025700091111     c                             (tratur = 'N' or tratur = *blank)
025800070606     c                   iter
025900070606     c                   endif
026000091217      *
026100091217     c                   else
026200091217      *
026300091217     c                   if        tradfi < vtradfig and vtradfi > 0 and
026400091217     c                             tradfi > 0
026500091217     c                   iter
026600091217     c                   endif
026700070606     c                   endif
026800070530      * sponda idraulica
026900070530     c                   if        vtraspi <> *blank
027000070530     c                   if        vtraspi = 'S' and traspi <> 'S' or
027100070530     c                             vtraspi = 'N' and traspi = 'S'
027200070530     c                   iter
027300070530     c                   endif
027400070531     c                   endif
027500070606      * pubblicità
027600070606     c                   if        vtrapub <> *blank
027700070606     c                   if        vtrapub = 'S' and trapub <> 'S' or
027800070619     c                             vtrapub = 'N' and trapub <> 'N' or
027900070619     c                             vtrapub = 'X' and trapub <> 'X' or
028000070619     c                             vtrapub = 'V' and trapub <> 'V'
028100070606     c                   iter
028200070606     c                   endif
028300070606     c                   endif
028400070619      * rifrangente
028500070619     c                   if        vtrarif <> *blank
028600070619     c                   if        vtrarif = 'S' and trarif <> 'S' or
028700070619     c                             vtrarif = 'N' and trarif <> 'N' or
028800070619     c                             vtrarif = 'X' and trarif <> 'X'
028900070619     c                   iter
029000070619     c                   endif
029100070619     c                   endif
029200070530      * allestimento
029300070530     c                   if        vtrafur <> *blank
029400070530     c                   if        vtrafur  <> trafur
029500070530     c                   iter
029600070530     c                   endif
029700070531     c                   endif
029800070530      * classe omologazione
029900070530     c                   if        vtraff1 <> *blank
030000070530     c                   if        vtraff1  <> traff1
030100070530     c                   iter
030200070530     c                   endif
030300070531     c                   endif
030400070530      * tipo alimentazione
030500070530     c                   if        vtraff2 <> *blank
030600070530     c                   if        vtraff2  <> traff2
030700070530     c                   iter
030800070530     c                   endif
030900070531     c                   endif
031000070530      * antifurto blocca porte
031100070530     c                   if        vtrabpo <> *blank
031200070530     c                   if        vtrabpo  <> trabpo
031300070530     c                   iter
031400070530     c                   endif
031500070531     c                   endif
031600070530      * antifurto blocco motore
031700070530     c                   if        vtraant <> *blank
031800070530     c                   if        vtraant  <> traant
031900070530     c                   iter
032000070530     c                   endif
032100070531     c                   endif
032200050801      * scrive W.F
032300050801     c                   move      orgcar        traarea
032400050801     c                   move      orgfl3        tradist
032500050801     c                   movel     tradass       traanno
032600151116     c* fornitore ragione sociale estesa
032700151116     C                   exsr      RagSocCont
032800151116     c                   eval      trasocl = RagSocKsc
032900151116      *
033000050801     c                   write     waitra00
033100121031      * scrive secondo file completo di decodifiche e campi in chiaro
033200121031     c                   exsr      secondoFile
033300121031     c                   write     waitra20
033400050623     c                   enddo
033500020304
033600020304     c                   endsr
033700981029      *
033800121031     c*-------------------------------------------------
033900121031     c     secondofile   begsr
034000121031     c*-------------------------------------------------
034100121031     c*decodifiche
034200121031     c*centrale satellitare
034300160112     c                   clear                   tracend
034400160112     c                   if        tracen  <> *blank
034500121031     c                   clear                   tibs02ds
034600121031     c                   movel     'C'           t02mod
034700121031     c                   movel     knsif         t02sif
034800121031     c                   movel     'CEN'         t02cod
034900160112     c                   movel     tracen        t02ke1
035000121031     c                   call      'TIBS02R'
035100121031     c                   parm                    KPJBA
035200121031     c                   parm                    TIBS02DS
035300121031     c                   if        t02err =  *blanks
035400160112     c                   movel     t02uni        tracend
035500121031     c                   endif
035600121031     c                   endif
035700121031      *vano carico
035800121031     c                   clear                   trabpod
035900121031     c                   if        trabpo <> *blank
036000121031     c                   clear                   tibs02ds
036100121031     c                   movel     'C'           t02mod
036200121031     c                   movel     knsif         t02sif
036300121031     c                   movel     'ANT'         t02cod
036400121031     c                   movel     'VAN'         t02ke1
036500121031     c                   movel     trabpo        t02ke2
036600121031     c                   call      'TIBS02R'
036700121031     c                   parm                    KPJBA
036800121031     c                   parm                    TIBS02DS
036900121031     c                   if        t02err =  *blanks
037000121031     c                   movel     t02uni        trabpod
037100121031     c                   endif
037200121031     c                   endif
037300121031      *blocco motore
037400121031     c                   clear                   traantd
037500121031     c                   if        traant <> *blank
037600121031     c                   clear                   tibs02ds
037700121031     c                   movel     'C'           t02mod
037800121031     c                   movel     knsif         t02sif
037900121031     c                   movel     'ANT'         t02cod
038000121105     c                   movel     'MOT'         t02ke1
038100121031     c                   movel     traant        t02ke2
038200121031     c                   call      'TIBS02R'
038300121031     c                   parm                    KPJBA
038400121031     c                   parm                    TIBS02DS
038500121031     c                   if        t02err =  *blanks
038600121031     c                   movel     t02uni        traantd
038700121031     c                   endif
038800121031     c                   endif
038900121031      *turnover
039000121031     c                   clear                   traturd
039100121031     c                   if        tratur <> *blank
039200121031     c                   clear                   tibs02ds
039300121031     c                   movel     'C'           t02mod
039400121031     c                   movel     knsif         t02sif
039500121031     c                   movel     'ATU'         t02cod
039600121031     c                   movel     tratur        t02ke1
039700121031     c                   call      'TIBS02R'
039800121031     c                   parm                    KPJBA
039900121031     c                   parm                    TIBS02DS
040000121031     c                   if        t02err =  *blanks
040100121031     c                   movel     t02uni        datu
040200121031     c                   movel     §AtuDES       traturd
040300121031     c                   endif
040400121031     c                   endif
040500121031      *allestimento
040600121031     c                   clear                   trafurd
040700121031     c                   if        trafur <> *blank
040800121031     c                   clear                   tibs02ds
040900121031     c                   movel     'C'           t02mod
041000121031     c                   movel     knsif         t02sif
041100121031     c                   movel     'ALL'         t02cod
041200121031     c                   movel     trafur        t02ke1
041300121031     c                   call      'TIBS02R'
041400121031     c                   parm                    KPJBA
041500121031     c                   parm                    TIBS02DS
041600121031     c                   if        t02err =  *blanks
041700121031     c                   movel     t02uni        trafurd
041800121031     c                   endif
041900121031     c                   endif
042000121031      *decodifica classe omologazione
042100121031     c                   clear                   traff1d
042200121031     c                   if        traff1 <> *blank
042300121031     c                   clear                   tibs02ds
042400121031     c                   movel     'C'           t02mod
042500121031     c                   movel     knsif         t02sif
042600121031     c                   movel     'OMO'         t02cod
042700121031     c                   movel     traff1        t02ke1
042800121031     c                   call      'TIBS02R'
042900121031     c                   parm                    KPJBA
043000121031     c                   parm                    TIBS02DS
043100121031     c                   if        t02err =  *blanks
043200121031     c                   movel     t02uni        traff1d
043300121031     c                   endif
043400121031     c                   endif
043500121031      *decodifica tipo alimentazione
043600121031     c                   clear                   traff2d
043700121031     c                   if        traff2 <> *blank
043800121031     c                   clear                   tibs02ds
043900121031     c                   movel     'C'           t02mod
044000121031     c                   movel     knsif         t02sif
044100121031     c                   movel     'ALI'         t02cod
044200121031     c                   movel     traff2        t02ke1
044300121031     c                   call      'TIBS02R'
044400121031     c                   parm                    KPJBA
044500121031     c                   parm                    TIBS02DS
044600121031     c                   if        t02err =  *blanks
044700121031     c                   movel     t02uni        traff2d
044800121031     c                   endif
044900121031     c                   endif
045000160113      *decodifica CCNL
045100160113     c                   clear                   traCNLd
045200160113     c                   if        traCNL <> *blank
045300160113     c                   clear                   tibs02ds
045400160113     c                   movel     'C'           t02mod
045500160113     c                   movel     knsif         t02sif
045600160113     c                   movel     'CNL'         t02cod
045700160113     c                   movel     traCNL        t02ke1
045800160113     c                   call      'TIBS02R'
045900160113     c                   parm                    KPJBA
046000160113     c                   parm                    TIBS02DS
046100160113     c                   if        t02err =  *blanks
046200160113     c                   movel     t02uni        traCNLd
046300160113     c                   endif
046400160113     c                   endif
046500160113      *decodifica QUALIFICA
046600160113     c                   clear                   traQUAd
046700160113     c                   if        traQUA <> *blank
046800160113     c                   clear                   tibs02ds
046900160113     c                   movel     'C'           t02mod
047000160113     c                   movel     knsif         t02sif
047100160113     c                   movel     'QUA'         t02cod
047200160113     c                   movel     traQUA        t02ke1
047300160113     c                   call      'TIBS02R'
047400160113     c                   parm                    KPJBA
047500160113     c                   parm                    TIBS02DS
047600160113     c                   if        t02err =  *blanks
047700160113     c                   movel     t02uni        traQUAd
047800160113     c                   endif
047900160113     c                   endif
048000160113      *decodifica LIVELLO
048100160113     c                   clear                   traLIQd
048200160113     c                   if        traLIQ <> *blank
048300160113     c                   clear                   tibs02ds
048400160113     c                   movel     'C'           t02mod
048500160113     c                   movel     knsif         t02sif
048600160113     c                   movel     'LIQ'         t02cod
048700160113     c                   movel     traLIQ        t02ke1
048800160113     c                   call      'TIBS02R'
048900160113     c                   parm                    KPJBA
049000160113     c                   parm                    TIBS02DS
049100160113     c                   if        t02err =  *blanks
049200160113     c                   movel     t02uni        traLIQd
049300160113     c                   endif
049400160113     c                   endif
049500121031      *des filiale
049600121031     c                   clear                   trafild
049700121031     c     trafil        chain     azorg01l
049800121031     c                   movel     orgdes        trafild
049900121031      *
050000121031      * descrizione area
050100121031     c                   clear                   traaread
050200121031     c                   movel     traarea       kkey
050300121031     c                   move      '05'          kcod
050400121031     c     ktab          chain     tabel00f
050500121031     c                   if        %found(tabel00f)
050600121031     c                   movel     tbluni        traaread
050700121031     c                   endif
050800121031     c*
050900121031     c                   clear                   tradistd
051000121031     c                   movel(p)  tradist       kkey
051100121031     c                   move      '17'          kcod
051200121031     c     ktab          chain     tabel00f
051300121031     c                   if        %found(tabel00f)
051400121031     c                   movel     tbluni        tradistd
051500121031     c                   endif
051600160113     c*
051700121031     c                   endsr
051800151116      * ?___________________________________________________________________
051900151116     C     RagSocCont    BegSR
052000151116      * ?___________________________________________________________________
052100151116      *
052200151116     C                   clear                   trmz70s1ds
052300151116     C                   movel(p)  TRaivas       PartitaIVA                     Input
052400151116     C                   movel(p)  'F'           SottoNatur                     Input "F/C"
052500151116     C                   movel(p)  TRasocg       Societa                        Input/Output
052600151116     C                   movel(p)  TRaFIL        Unita                          Input/Output
052700151116      *
052800151116     c                   call      'TRMZ70SR1'
052900151116     C                   parm                    trmz70s1ds
053000151116     c*
053100151116     C                   ENDSR
053200050802     c*-------------------------------------------------
053300050802     c     waitra        begsr
053400050802     c*-------------------------------------------------
053500070530     c                   clear                   totmzfil
053600070530     c                   clear                   totmzarea
053700070530     c                   clear                   totmzdis
053800070530     c                   clear                   totmzaz
053900070530     c                   clear                   primo
054000070530     c     *loval        setll     waitra0f
054100070530     c                   do        *hival
054200070530     c                   read      waitra0f                               98
054300070530     c   98              leave
054400070530      * filtro area (solo x sede)
054500070530     c                   if        varea <> *blank
054600070530     c                   move      traarea       traareaa          3
054700070530     c                   if        traareaa <> varea
054800070530     c                   iter
054900070530     c                   endif
055000070530     c                   endif
055100070530      * filtro distretto (solo x sede)
055200070530     c                   if        vdist <> *blank and tradist <> vdist
055300070530     c                   iter
055400070530     c                   endif
055500070530      *
055600070530     c                   if        primo = *blank
055700070530     c                   move      trafil        exfil
055800070530     c                   move      traarea       excar
055900070530     c                   move      tradist       exfl3
056000070530     c                   move      'X'           primo
056100070530     c                   endif
056200070530      * rottura filiale
056300070530     c                   if        trafil  <> exfil
056400091216     c                   if        vstampa = 'S'
056500070530     c                   if        line > 60
056600070530     c                   except    testafil
056700070530     c                   endif
056800091216     c                   endif
056900100114      *
057000070530     c     exfil         chain     azorg01l
057100091216     c                   if        vstampa = 'S'
057200070530     c                   except    totfil
057300091216     c                   endif
057400070530     c                   clear                   totmzfil
057500070530     c                   move      trafil        exfil
057600070530     c                   endif
057700070530      *
057800070530      * rottura area
057900070530     c                   if        traarea <> excar
058000091216     c                   if        vstampa = 'S'
058100070530     c                   if        line > 60
058200070530     c                   except    testafil
058300070530     c                   endif
058400091216     c                   endif
058500091216      *
058600070530      * descrizione area
058700070530     c                   clear                   exaread20
058800070530     c                   movel     excar         kkey
058900070530     c                   move      '05'          kcod
059000070530     c     ktab          chain     tabel00f
059100070530     c                   if        %found(tabel00f)
059200070530     c                   movel     tbluni        exaread20
059300070530     c                   endif
059400091216      *
059500091216     c                   if        vstampa = 'S'
059600070530     c                   except    detarea
059700091216     c                   endif
059800070530     c                   clear                   totmzarea
059900070530     c                   move      traarea       excar
060000070530     c   66              z-add     99            line
060100070530     c                   endif
060200070530      *
060300070530      * rottura distretto
060400070530     c                   if        tradist <> exfl3
060500091216     c                   if        vstampa = 'S'
060600070530     c                   if        line > 60
060700070530     c                   except    testafil
060800070530     c                   endif
060900091216     c                   endif
061000091216      *
061100070530     c                   clear                   exdistd20
061200070530     c                   movel(p)  exfl3         kkey
061300070530     c                   move      '17'          kcod
061400070530     c     ktab          chain     tabel00f
061500070530     c                   if        %found(tabel00f)
061600070530     c                   movel     tbluni        exdistd20
061700070530     c                   endif
061800091216      *
061900091216     c                   if        vstampa = 'S'
062000091216      *
062100070530     c                   except    detdist
062200091216      *
062300091216     c                   endif
062400091216      *
062500070530     c                   clear                   totmzdis
062600070530     c                   move      tradist       exfl3
062700070530     c   66              z-add     99            line
062800070530     c                   endif
062900070530      * dettaglio  p.o
063000091216      *
063100091216     c                   if        vstampa = 'S'
063200091216      *
063300070530     c                   if        line > 60
063400070530     c                   except    testafil
063500070530     c                   endif
063600091216      *
063700091216     c                   endif
063800091216      *
063900070530     c     exfil         chain     azorg01l
064000070606     c                   clear                   tradfis
064100070606     c                   setoff                                       99
064200070606      * data disaccreditamento da stampare per turnover
064300091111     c                   if        tradfi > 0  and vtradfi > 0
064400070606     c                   seton                                        99
064500070606     c                   move      tradfi        dataiso
064600070606     c                   move      dataiso       dataeur
064700070606     c                   move      dataeur       tradfis           8 0
064800070606     c                   endif
064900091216      *
065000091216     c                   if        vstampa = 'S'
065100091216      *
065200070530     c                   except    detfil
065300091216      *
065400091216     c                   end
065500091216      *
065600070530      * incremento totale mezzi per area e filiale
065700070530     c                   add       1             totmzfil
065800070530     c                   add       1             totmzarea
065900070530     c                   add       1             totmzdis
066000070530     c                   add       1             totmzaz
066100070530
066200070530     c                   enddo
066300070530
066400070530      * fine file controlli finali x stampa totali
066500091216      *
066600091216     c                   if        vstampa = 'S'
066700091216      *
066800070531     c*                  if        lisarea = 'S' and *in80 or
066900070531     c*                            lisdist = 'S' and *in81
067000070530     c                   if        line > 60
067100070530     c                   except    testafil
067200070530     c                   endif
067300091216      *
067400091216     c                   endif
067500091216      *
067600070530      * descrizione area
067700070530     c                   clear                   exaread20
067800070530     c                   movel     excar         kkey
067900070530     c                   move      '05'          kcod
068000070530     c     ktab          chain     tabel00f
068100070530     c                   if        %found(tabel00f)
068200070530     c                   movel     tbluni        exaread20
068300070530     c                   endif
068400091216      *
068500091216     c                   if        vstampa = 'S'
068600091216      *
068700070530     c                   except    totfil
068800091216      *
068900091216     c                   end
069000091216      *
069100070530      * stampo totale area finale
069200091216      *
069300091216     c                   if        vstampa = 'S'
069400091216      *
069500070530     c                   if        *in80 or *in81 or *in82 and v1cfil = 0
069600070530     c                   except    detarea
069700070530     c                   endif
069800091216      *
069900091216     c                   endif
070000091216      *
070100070530      * stampo totale distretto finale
070200070530     c                   if        *in81 or *in82 and v1cfil = 0
070300070530     c                   clear                   exdistd20
070400070530     c                   movel(p)  exfl3         kkey
070500070530     c                   move      '17'          kcod
070600070530     c     ktab          chain     tabel00f
070700070530     c                   if        %found(tabel00f)
070800070530     c                   movel     tbluni        exdistd20
070900070530     c                   endif
071000091216      *
071100091216     c                   if        vstampa = 'S'
071200091216      *
071300070530     c                   except    detdist
071400091216      *
071500091216     c                   endif
071600091216      *
071700070530     c                   endif
071800070530      * stampo totale aziendale solo se richiesta globale di sede
071900091216      *
072000091216     c                   if        vstampa = 'S'
072100091216      *
072200070530     c                   if        *in82 and v1cfil = 0
072300070530     c                   except    detaz
072400070530     c                   endif
072500091216      *
072600091216     c                   endif
072700091216      *
072800070530
072900070531     c*                  endif
073000070530     c                   endsr
073100070530      *
073200050802      *
073300031105     c*-------------------------------------------------
073400031105     c     controv1      begsr
073500031105     c*-------------------------------------------------
073600031105     c                   clear                   v1dfil
073700070606     c                   setoff                                       9697
073800051122     c                   setoff                                       959493
073900051122     c                   setoff                                       92
074000031105     c                   if        v1cfil > 0
074100051122
074200050623     c     v1cfil        chain     azorg01l
074300050623     c                   if        %found(azorg01l)
074400031105     c                   movel     orgdes        v1dfil
074500031105     c                   else
074600031105     c                   seton                                        9796
074700051122     c                   goto      endcontr
074800031105     c                   endif
074900051122      * se profilo di area verifico se area di appartenenza
075000051122     c                   if        *in80
075100051122     c                   if        orgcar <> o36car
075200051122     c                   seton                                        9396
075300051122     c                   goto      endcontr
075400051122     c                   endif
075500051122     c                   endif
075600051122      * se profilo di distretto verifico se di appartenenza
075700051122     c                   if        *in81
075800051122     c                   if        orgfl3 <> o36dis
075900051122     c                   seton                                        9296
076000051122     c                   goto      endcontr
076100051122     c                   endif
076200051122     c                   endif
076300051122      *
076400031105     c                   endif
076500051122      * controllo congruenza selezioni a video
076600051122     c                   if        v1cfil > 0 and
076700051122     c                             (varea <> *blank or vdist <> *blank)
076800051122     c                   seton                                        9596
076900051122     c                   goto      endcontr
077000051122     c                   endif
077100051122      *
077200051122     c                   if        varea <> *blank and vdist <> *blank
077300051122     c                   seton                                        9496
077400051122     c                   goto      endcontr
077500051122     c                   endif
077600051121      * x sede parzializza area o distretto
077700051121     C     varea         ifeq      '?'
077800051121     C                   movel     '05'          kcod
077900051121     C                   clear                   kkey
078000051121     C                   call      'X§TABER'
078100051121     C                   parm                    kkut
078200051121     C                   parm                    kcod
078300051121     C                   parm                    kkey
078400051121     C                   parm                    §des             30
078500051121     c                   movel     §des          varead
078600051121     c                   movel     kkey          varea
078700051121     C*
078800051121     C                   else
078900051121     C*
079000051121     C                   if        varea <> *blank and varea <> *zero
079100051121     C                   movel     '05'          kcod
079200051121     C                   movel(p)  varea         kkey
079300051121     C     ktab          chain     tabel00f
079400051121     c                   if        %found(tabel00f) and tblflg = *blank
079500051121     C                   movel     tbluni        varead
079600051121     c                   else
079700051121     c                   movel     *all'?'       varead
079800051121     C                   end
079900051121     C                   end
080000051121     C                   end
080100051121      * distretto
080200051121     C     vdist         ifeq      '?'
080300051121     C                   movel     '17'          kcod
080400051121     C                   clear                   kkey
080500051121     C                   call      'X§TABER'
080600051121     C                   parm                    kkut
080700051121     C                   parm                    kcod
080800051121     C                   parm                    kkey
080900051121     C                   parm                    §des
081000051121     c                   movel     §des          vdistd
081100051121     c                   movel     kkey          vdist
081200051121     C*
081300051121     C                   else
081400051121     C*
081500051121     C                   if        vdist <> *blank and vdist <> *zero
081600051121     C                   movel     '17'          kcod
081700051121     C                   movel(p)  vdist         kkey
081800051121     C     ktab          chain     tabel00f
081900051121     c                   if        %found(tabel00f) and tblflg = *blank
082000051121     C                   movel     tbluni        vdistd
082100051121     c                   else
082200051121     c                   movel     *all'?'       vdistd
082300051121     C                   end
082400051121     C                   end
082500051121     C                   end
082600070605      *turnover
082700070605     c                   setoff                                       43
082800070605     c                   if        vtratur <> *blank
082900070605     c                   if        vtrasco <> *blank or
083000070605     c                             vtratmp <> *blank or
083100070605     c                             vtrafur <> *blank or
083200070605     c                             vtrapub <> *blank or
083300070619     c                             vtrarif <> *blank or
083400070605     c                             vtraant <> *blank or
083500070605     c                             vtrabpo <> *blank or
083600070605     c                             vtraff1 <> *blank or
083700070605     c                             vtraff2 <> *blank or
083800070605     c                             vtraspi <> *blank or
083900091006     c                             vtraris <> *blank or
084000120914     c                             vtraloci <> *blank or
084100091006     c                             vtracor <> *blank
084200070605     c                   seton                                        4396
084300070605     c                   goto      endcontr
084400070605     c                   endif
084500070605     c                   endif
084600070605      *data disaccr. per turnover
084700070605     c                   setoff                                       44
084800070605     c                   if        vtradfi = 0  and vtratur <> *blank
084900070605     c                   seton                                        4496
085000070605     c                   goto      endcontr
085100070605     c                   endif
085200091216      *
085300070605     c                   setoff                                       41
085400091216      * il controllo deve essere attivo solo per utenti di filiale :
085500091216      * se immessa una data deve esserci anche il flag di TURNOVER ='S'
085600091216     c                   if        not *in82
085700091216     c                   if        vtradfi > 0  and vtratur = *blank
085800091216     c                   seton                                        4196
085900091216     c                   goto      endcontr
086000091216     c                   endif
086100091216     c                   endif
086200091216      *
086300070605     c                   if        vtradfi > 0
086400070605     C                   MOVE      Vtradfi       G02DAT
086500070605     C                   MOVEL     *BLANK        G02ERR
086600070605     C                   CALL      'XSRDA8'
086700070605     C                   PARM                    WLBDA8
086800070605     C     G02ERR        IFEQ      '1'
086900070605     C                   SETON                                        4295
087000070605     c                   goto      endcontr
087100070605     C                   END
087200070605     c                   move      g02dat        vtradfi
087300070605     c                   move      g02inv        vtradfig          8 0
087400070605     c                   endif
087500070529      *allestimento (tabella)
087600070529     c                   setoff                                       45
087700070529     c                   if        vtrafur = '?'
087800070529     c                   clear                   tibs02ds
087900070529     c                   movel     'R'           t02mod
088000070529     c                   movel     knsif         t02sif
088100070529     c                   movel     'ALL'         t02cod
088200070529     c                   call      'TIBS02R'
088300070529     c                   parm                    KPJBA
088400070529     c                   parm                    TIBS02DS
088500070529     c                   if        t02err =  *blanks
088600070529     c                   movel     t02uni        vtrafurd
088700070529     c                   movel     t02ke1        vtrafur
088800070529     c                   goto      endcontr
088900070529     c                   endif
089000070529     c                   else
089100070529     c                   if        vtrafur <> *blank
089200070529     c                   clear                   tibs02ds
089300070529     c                   movel     'C'           t02mod
089400070529     c                   movel     knsif         t02sif
089500070529     c                   movel     'ALL'         t02cod
089600070529     c                   movel     vtrafur       t02ke1
089700070529     c                   call      'TIBS02R'
089800070529     c                   parm                    KPJBA
089900070529     c                   parm                    TIBS02DS
090000070529     c                   if        t02err =  *blanks
090100070529     c                   movel     t02uni        vtrafurd
090200070529     c                   else
090300070529     c                   seton                                        4596
090400070529     c                   goto      endcontr
090500070529     c                   endif
090600070529     c                   endif
090700070529     c                   endif
090800070529      * antifurto vano carico
090900070529     c                   setoff                                       46
091000070529     c                   if        vtrabpo = '?  '
091100070529     c                   clear                   tibs02ds
091200070529     c                   movel     'R'           t02mod
091300070529     c                   movel     knsif         t02sif
091400070529     c                   movel     'ANT'         t02cod
091500070529     c                   movel(p)  'VAN'         t02ke1
091600070529     c                   call      'TIBS02R'
091700070529     c                   parm                    KPJBA
091800070529     c                   parm                    TIBS02DS
091900070529     c                   if        t02err =  *blanks
092000070529     c                   movel     t02uni        vtrabpod
092100070529     c                   movel     t02ke2        vtrabpo
092200070529     c                   endif
092300070529     c                   else
092400070529     c                   if        vtrabpo <> *blank
092500070529     c                   clear                   tibs02ds
092600070529     c                   movel     'C'           t02mod
092700070529     c                   movel     knsif         t02sif
092800070529     c                   movel     'ANT'         t02cod
092900070529     c                   movel     'VAN'         t02ke1
093000070529     c                   movel     vtrabpo       t02ke2
093100070529     c                   call      'TIBS02R'
093200070529     c                   parm                    KPJBA
093300070529     c                   parm                    TIBS02DS
093400070529     c                   if        t02err =  *blanks
093500070529     c                   movel     t02uni        vtrabpod
093600070529     c                   else
093700070529     c                   seton                                        4696
093800070529     c                   goto      endcontr
093900070529     c                   endif
094000070529     c                   endif
094100070529     c                   endif
094200070529      * antifurto blocco motore
094300070529     c                   setoff                                       47
094400070529     c                   if        vtraant = '?  '
094500070529     c                   clear                   tibs02ds
094600070529     c                   movel     'R'           t02mod
094700070529     c                   movel     knsif         t02sif
094800070529     c                   movel     'ANT'         t02cod
094900070529     c                   movel(p)  'MOT'         t02ke1
095000070529     c                   call      'TIBS02R'
095100070529     c                   parm                    KPJBA
095200070529     c                   parm                    TIBS02DS
095300070529     c                   if        t02err =  *blanks
095400070529     c                   movel     t02uni        vtraantd
095500070529     c                   movel     t02ke2        vtraant
095600070529     c                   endif
095700070529     c                   else
095800070529     c                   if        vtraant <> *blank
095900070529     c                   clear                   tibs02ds
096000070529     c                   movel     'C'           t02mod
096100070529     c                   movel     knsif         t02sif
096200070529     c                   movel     'ANT'         t02cod
096300070529     c                   movel     'MOT'         t02ke1
096400070529     c                   movel     vtraant       t02ke2
096500070529     c                   call      'TIBS02R'
096600070529     c                   parm                    KPJBA
096700070529     c                   parm                    TIBS02DS
096800070529     c                   if        t02err =  *blanks
096900070529     c                   movel     t02uni        vtraantd
097000070529     c                   else
097100070529     c                   seton                                        4796
097200070529     c                   goto      endcontr
097300070529     c                   endif
097400070529     c                   endif
097500070529     c                   endif
097600070529      *Classe omologazione (tabella)
097700070529     c                   setoff                                       31
097800070529     c                   clear                   vtraff1d
097900070529     c                   if        vtraff1 = '?'
098000070529     c                   clear                   tibs02ds
098100070529     c                   movel     'R'           t02mod
098200070529     c                   movel     knsif         t02sif
098300070529     c                   movel     'OMO'         t02cod
098400070529     c                   call      'TIBS02R'
098500070529     c                   parm                    KPJBA
098600070529     c                   parm                    TIBS02DS
098700070529     c                   if        t02err =  *blanks
098800070529     c                   movel     t02uni        vtraff1d
098900070529     c                   movel     t02ke1        vtraff1
099000070529     c                   goto      endcontr
099100070529     c                   endif
099200070529     c                   else
099300070529     c                   if        vtraff1 <> *blank
099400070529     c                   clear                   tibs02ds
099500070529     c                   movel     'C'           t02mod
099600070529     c                   movel     knsif         t02sif
099700070529     c                   movel     'OMO'         t02cod
099800070529     c                   movel     vtraff1       t02ke1
099900070529     c                   call      'TIBS02R'
100000070529     c                   parm                    KPJBA
100100070529     c                   parm                    TIBS02DS
100200070529     c                   if        t02err =  *blanks
100300070529     c                   movel     t02uni        vtraff1d
100400070529     c                   else
100500070529     c                   seton                                        3195
100600070529     c                   goto      endcontr
100700070529     c                   endif
100800070529     c                   endif
100900070529     c                   endif
101000070529      *tipo alimentazione (tabella)
101100070529     c                   setoff                                       32
101200070529     c                   clear                   vtraff2d
101300070529     c                   if        vtraff2 = '?'
101400070529     c                   clear                   tibs02ds
101500070529     c                   movel     'R'           t02mod
101600070529     c                   movel     knsif         t02sif
101700070529     c                   movel     'ALI'         t02cod
101800070529     c                   call      'TIBS02R'
101900070529     c                   parm                    KPJBA
102000070529     c                   parm                    TIBS02DS
102100070529     c                   if        t02err =  *blanks
102200070529     c                   movel     t02uni        vtraff2d
102300070529     c                   movel     t02ke1        vtraff2
102400070529     c                   goto      endcontr
102500070529     c                   endif
102600070529     c                   else
102700070529     c                   if        vtraff2 <> *blank
102800070529     c                   clear                   tibs02ds
102900070529     c                   movel     'C'           t02mod
103000070529     c                   movel     knsif         t02sif
103100070529     c                   movel     'ALI'         t02cod
103200070529     c                   movel     vtraff2       t02ke1
103300070529     c                   call      'TIBS02R'
103400070529     c                   parm                    KPJBA
103500070529     c                   parm                    TIBS02DS
103600070529     c                   if        t02err =  *blanks
103700070529     c                   movel     t02uni        vtraff2d
103800070529     c                   else
103900070529     c                   seton                                        3295
104000070529     c                   goto      endcontr
104100070529     c                   endif
104200070529     c                   endif
104300070529     c                   endif
104400051121
104500051122     c     endcontr      endsr
104600050802     c*-------------------------------------------------
104700050802     c     comandi       begsr
104800050802     c*-------------------------------------------------
104900050802     C                   close     waitra0f
105000050802      *
105100070608     C                   z-add     99            luncmd
105200050802     C                   CALL      'QCMDEXC'
105300050802     C                   parm                    coman
105400050802     C                   parm                    luncmd
105500050802      *
105600070601     C  nkc              OPEN      waitra0f
105700050802     c                   endsr
105800070608     c*-------------------------------------------------
105900070608     c     comandi2      begsr
106000070608     c*-------------------------------------------------
106100070608      *
106200070608     C                   z-add     99            luncmd
106300070608     C                   CALL      'QCMDEXC'                            34
106400070608     C                   parm                    coman
106500070608     C                   parm                    luncmd
106600070608      *
106700070608     c                   endsr
106800121002     c*-------------------------------------------------
106900121002     c     comandiB      begsr
107000121002     c*-------------------------------------------------
107100121105     C                   close     waitra2f
107200121002      *
107300121002     C                   z-add     99            luncmd
107400121002     C                   CALL      'QCMDEXC'
107500121002     C                   parm                    coman
107600121002     C                   parm                    luncmd
107700121002      *
107800121105     C  nkc              OPEN      waitra2f
107900121002     c                   endsr
108000011026     c*-------------------------------------------------
108100011026     c     *inzsr        begsr
108200011026     c*-------------------------------------------------
108300020304     c     *entry        plist
108400020304     c                   parm                    kpjba
108500050801
108600050801     c     ktab          klist
108700050801     c                   kfld                    kkut              1 0
108800050801     c                   kfld                    kcod              2
108900050801     c                   kfld                    kkey              8
109000050801     c                   z-add     1             kkut
109100050801     c                   move      '05'          kcod
109200050801     c
109300050801
109400020218     C                   time                    w0120            14 0
109500020218     C                   move      w0120         wdat              8 0
109600020404     c                   z-add     99            line
109700030630      *profilo di filiale estraggo solo quelli relativi al P.O.
109800030630     C                   CLEAR                   Tibs36Ds
109900030630     C                   EVAL      I36ute = knmus
110000030630     C                   CALL      'TIBS36R'
110100030630     C                   PARM                    tibs36ds
110200051017      *se profilo di filiale per pilotare le letture solo sul P.O
110300050802     c                   clear                   lisarea
110400050802     c                   clear                   lisdist
110500080502     c                   setoff                                       84
110600051017     c                   eval      saltp = 'N'
110700080502      *
110800051017      * x filiale duplica file di work in qtemp
110900051017     c                   movea     cmd2          coman
111000051017     c                   exsr      comandi
111100121002     c                   movea     cmd8          coman
111200121002     c                   exsr      comandib
111300080502      *
111400030630     c                   if        o36pou <> 046
111500050805     c                   move      o36pou        v1cfil
111600080502      *
111700050802      * verifica se profili di filiale con autorizzazione a area o distretto
111800050802     c                   if        %subst(knmus:1:3) ='REA'
111900050802     c                   seton                                        80
112000050802     c                   move      'S'           lisarea
112100050802     c                   endif
112200050802     c
112300050802     c                   if        %subst(knmus:1:3) ='RED'
112400050802     c                   seton                                        81
112500050802     c                   move      'S'           lisdist
112600050802     c                   endif
112700080502     c
112800080502     c                   if        %subst(knmus:1:3) ='COA'
112900080502     c                   seton                                        84
113000080502     c                   endif
113100080502      *
113200051018     c                   else
113300051018      *profilo di sede
113400051018     c                   seton                                        82
113500030630     c                   endif
113600050802
113700050805     c                   if        v1cfil <> 0
113800031105     c     v1cfil        chain     azorg01l
113900031105     c                   if        %found(azorg01l)
114000031105     c                   movel     orgdes        v1dfil
114100050802     c                   move      orgcar        warea
114200050802     c                   move      orgfl3        wdist
114300031105     c                   endif
114400050805     c                   endif
114500070608      * creo nome file per dati utente in libreria di wrk
114600070608     c                   eval      file = 'AU' + knmus
114700121105     c                   eval      file2 = 'AW' + knmus
114800011026     c                   endsr
114900050802      *---------------------------------------------------------
115000070531     Oqsysprt   E            legenda        1 02
115100090326     o                                           10 'TRMZ74R'
115200070531     o                       knmus               22
115300070606     o                                        +   1 'Lista Automezzi/Autisti -
115400070606     o                                              di città  PARAMETRI'
115500070531     o                       udate         y  +   3
115600070531     O                                        +   1 'Pag.'
115700070531     O                       page          z  +   1
115800070531     O          E            legenda     2
115900070531     o                                            8 'Filiale'
116000070531     o                       v1cfil           +   2
116100070531     o                       v1dfil           +   1
116200070531     o                                        +   2 'Tutta l''area'
116300070531     o                       lisarea          +   1
116400070531     o                                        +   2 'Tutto distretto'
116500070531     o                       lisdist          +   1
116600070531     o                                        +   2 'Solo area'
116700070531     o                       varea            +   1
116800070531     o                       varead           +   1
116900070531     o                                        +   2 'Solo distr.'
117000070531     o                       vdist            +   1
117100070531     o                       vdistd           +   1
117200070531     O          E            legenda     0
117300070531     o                                            8 'Filiale'
117400070531     o                       v1cfil           +   2
117500070531     o                       v1dfil           +   1
117600070531     o                                        +   2 'Tutta l''area'
117700070531     o                       lisarea          +   1
117800070531     o                                        +   2 'Tutto distretto'
117900070531     o                       lisdist          +   1
118000070531     o                                        +   2 'Solo area'
118100070531     o                       varea            +   1
118200070531     o                       varead           +   1
118300070531     o                                        +   2 'Solo distr.'
118400070531     o                       vdist            +   1
118500070531     o                       vdistd           +   1
118600070531     O          E            legenda     0
118700070531     o                                            8 'Filiale'
118800070531     o                       v1cfil           +   2
118900070531     o                       v1dfil           +   1
119000070531     o                                        +   2 'Tutta l''area'
119100070531     o                       lisarea          +   1
119200070531     o                                        +   2 'Tutto distretto'
119300070531     o                       lisdist          +   1
119400070531     o                                        +   2 'Solo area'
119500070531     o                       varea            +   1
119600070531     o                       varead           +   1
119700070531     o                                        +   2 'Solo distr.'
119800070531     o                       vdist            +   1
119900070531     o                       vdistd           +   1
120000081204     O          E            legenda     2
120100081204     o                                           50 'Tipo Autista ..........'
120200081204     o                       vtpaut           +   2
120300070605     O          E            legenda     2
120400070605     o                                           50 'Turnover...............'
120500070605     o                       vtratur          +   2
120600070605     o                                        +   2 'Dalla data'
120700070605     o                       vtradfi          +   2 '  /  /    '
120800070531     O          E            legenda     2
120900170221     o                                           50 'Senza automezzo .......'
121000070531     o                       vtraris          +   2
121100100114     O          E            legenda     2
121200091006     o                                           50 'A perdere .............'
121300091006     o                       vtracor          +   2
121400070531     O          E            legenda     2
121500070531     o                                           50 'Mezzo di scorta........'
121600070531     o                       vtrasco          +   2
121700120914     O          E            legenda     2
121800120914     o                                           50 'Centro St/Loc.Turistiche'
121900120914     o                       vtraloci         +   2
122000070531     O          E            legenda     2
122100070531     o                                           50 'Colore automezzo.......'
122200070531     o                       vtratmp          +   2
122300070605     O          E            legenda     2
122400070605     o                                           50 'Pubblicità.............'
122500070605     o                       vtrapub          +   2
122600070619     O          E            legenda     2
122700070619     o                                           50 'Rifrangente............'
122800070619     o                       vtrarif          +   2
122900070531     O          E            legenda     2
123000070531     o                                           50 'Sponda idraulica.......'
123100070531     o                       vtraspi          +   2
123200070531     O          E            legenda     2
123300070531     o                                           50 'Allestimento ..........'
123400070531     o                       vtrafur          +   2
123500070531     o                       vtrafurd         +   2
123600070531     O          E            legenda     2
123700070531     o                                           50 'Classe omologazione....'
123800070531     o                       vtraff1          +   2
123900070531     o                       vtraff1d         +   2
124000070531     O          E            legenda     2
124100070531     o                                           50 'Tipo alimentazione.....'
124200070531     o                       vtraff2          +   2
124300070531     o                       vtraff2d         +   2
124400070531     O          E            legenda     2
124500070531     o                                           50 'Antifurto blocca porte.'
124600070531     o                       vtrabpo          +   2
124700070531     o                       vtrabpod         +   2
124800070531     O          E            legenda     2
124900070531     o                                           50 'Antifurto blocco motore'
125000070531     o                       vtraant          +   2
125100070531     o                       vtraantd         +   2
125200070531      *----------------------------------------------------------
125300070531     O          E            testafil       1 02
125400090326     o                                           10 'TRMZ74R'
125500050802     o                       knmus               22
125600070606     o                                        +   1 'Lista Automezzi/Autisti -
125700070606     o                                              di città ELENCO  '
125800050802     o                       udate         y  +   3
125900050802     O                                        +   1 'Pag.'
126000050802     O                       page          z  +   1
126100050803     O          E            testafil    2  1
126200050803     o                                           16 'Societa'
126300050803     o                                           42 'Autista'
126400050803     o                                           66 'Mezzo'
126500050803     o                                           84 'Targa'
126600070605     o                                          102 'Data Disaccr.'
126700050802     O          E            detfil      1
126800051122     o                       TRASOC           +   9
126900050802     o                       TRAAUT           +   1
127000050802     o                       TRATIA           +   1
127100050802     o                       TRASCO           +   1
127200050802     o                       TRATAA           +   1
127300070605     o               99      TRAdfis            100 '  /  /    '
127400050803     O          E            totfil      1  1
127500050803     o                                              'Totale P.O '
127600050803     o                       exfil            +   1
127700051121     o                       orgdes           +   1
127800051122     o                       totmzfil      z  +   4
127900050803     O          E            detarea     1  1
128000050803     o                                              'Totale area'
128100050803     o                       excar            +   1
128200051121     o                       exaread20        +   1
128300051122     o                       totmzarea     z  +   4
128400050803     O          E            detdist     1
128500050803     o                                              'Totale distretto'
128600050803     o                       exfl3            +   1
128700051121     o                       exdistd20        +   1
128800051121     o                       totmzdis      z  +   1
128900051018     O          E            detaz       1
129000051122     o                                              'Totale azienda'
129100051122     o                       totmzaz       z     45
129200050802**  CMD1
129300050802CLRPFM    FILE(WAITRA0F)
129400050802**  CMD2
129500050802CRTDUPOBJ OBJ(WAITRA0F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
129600050802**  CMD3
129700050802DLTF FILE(QTEMP/WAITRA0F)
129800070608**  CMD4
129900160115DLTF   FILE(WRKQUERY/xxxxxxxxxx)
130000070608**  CMD5
130100070608CRTDUPOBJ OBJ(WAITRA0F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(xxxxxxxxxx) DATA(*YES)
130200121002**  CMD7
130300121105CLRPFM    FILE(WAITRA2F)
130400121002**  CMD8
130500121105CRTDUPOBJ OBJ(WAITRA2F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
130600121002**  CMD9
130700121105DLTF FILE(QTEMP/WAITRA2F)
130800121002**  CMD11
130900121105CRTDUPOBJ OBJ(WAITRA2F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(xxxxxxxxxx) DATA(*YES)
