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
001200170808     Fprtf198   O    F  198        PRINTER prtctl(prtds)
001300020304     dkpjba          e ds
001301170901     D TIBS34DS      E DS                                                       *Profili utente
001302170901     D DDATIUTE      E DS                                                       *Dati utente
001303170901     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
001304170901     D dute01        E DS                                                       *Utenti
001305170901     d dLat          e ds
001306170901     D TRUL31DS      E DS
001307170901     D TRUL31DS2     E DS
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
005801170901      *
005802170901    dd wabi            s                   like(UteAut)
005803170901     d* p.o. abilitati
005804170901     D $pogn           s              3  0 dim(250)
005805170901     D $pog            s              3    dim(250)
005806170901     D $DIG            s              1    dim(20)
005807170901     D $ARG            s              3    dim(50)
005808170901     D
005900000000     C*---------------------------------------------------------------*
006000020304      *
006100031105     c                   do        *hival
006200031105     c                   exfmt     video1
006300031105     c                   clear                   $msg
006301170901     c                   setoff                                       28
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
023900170626      * Picco di Lavoro
024000170626     c                   if        vtraatp <> *blank
024100170626     c                   if        vtraatp = 'S' and traatp = *blank or
024200170626     c                             vtraatp = 'N' and traatp <> *blank
024300170626     c                   iter
024400170626     c                   endif
024500170626     c                   endif
024600120914      * Mezzi CENTRO ST/LOC.TURISTICHE
024700120914     c                   if        vtraloci <> *blank
024800120914     c                   if        vtraloci = 'S' and traloci = *blank
024900120914     c                             or
025000120914     c                             vtraloci = 'N' and traloci <> *blank
025100120914     c                   iter
025200120914     c                   endif
025300120914     c                   endif
025400070530      * colore automezzo
025500070530     c                   if        vtratmp <> *blank
025600070530     c                   if        vtratmp = 'S' and tratmp <> 'S'  or
025700070530     c                             vtratmp = 'N' and tratmp = 'S'
025800070530     c                   iter
025900070530     c                   endif
026000070531     c                   endif
026100070606      * turnover
026200070606     c                   if        vtratur <> *blank
026300091111     c                   if        tradfi < vtradfig or
026400091111     c                             (tratur = 'N' or tratur = *blank)
026500070606     c                   iter
026600070606     c                   endif
026700091217      *
026800091217     c                   else
026900091217      *
027000091217     c                   if        tradfi < vtradfig and vtradfi > 0 and
027100091217     c                             tradfi > 0
027200091217     c                   iter
027300091217     c                   endif
027400070606     c                   endif
027500070530      * sponda idraulica
027600070530     c                   if        vtraspi <> *blank
027700070530     c                   if        vtraspi = 'S' and traspi <> 'S' or
027800070530     c                             vtraspi = 'N' and traspi = 'S'
027900070530     c                   iter
028000070530     c                   endif
028100070531     c                   endif
028200070606      * pubblicità
028300070606     c                   if        vtrapub <> *blank
028400070606     c                   if        vtrapub = 'S' and trapub <> 'S' or
028500070619     c                             vtrapub = 'N' and trapub <> 'N' or
028600070619     c                             vtrapub = 'X' and trapub <> 'X' or
028700070619     c                             vtrapub = 'V' and trapub <> 'V'
028800070606     c                   iter
028900070606     c                   endif
029000070606     c                   endif
029100070619      * rifrangente
029200070619     c                   if        vtrarif <> *blank
029300070619     c                   if        vtrarif = 'S' and trarif <> 'S' or
029400070619     c                             vtrarif = 'N' and trarif <> 'N' or
029500070619     c                             vtrarif = 'X' and trarif <> 'X'
029600070619     c                   iter
029700070619     c                   endif
029800070619     c                   endif
029900070530      * allestimento
030000070530     c                   if        vtrafur <> *blank
030100070530     c                   if        vtrafur  <> trafur
030200070530     c                   iter
030300070530     c                   endif
030400070531     c                   endif
030500070530      * classe omologazione
030600070530     c                   if        vtraff1 <> *blank
030700070530     c                   if        vtraff1  <> traff1
030800070530     c                   iter
030900070530     c                   endif
031000070531     c                   endif
031100070530      * tipo alimentazione
031200070530     c                   if        vtraff2 <> *blank
031300070530     c                   if        vtraff2  <> traff2
031400070530     c                   iter
031500070530     c                   endif
031600070531     c                   endif
031700070530      * antifurto blocca porte
031800070530     c                   if        vtrabpo <> *blank
031900070530     c                   if        vtrabpo  <> trabpo
032000070530     c                   iter
032100070530     c                   endif
032200070531     c                   endif
032300070530      * antifurto blocco motore
032400070530     c                   if        vtraant <> *blank
032500070530     c                   if        vtraant  <> traant
032600070530     c                   iter
032700070530     c                   endif
032800070531     c                   endif
032900050801      * scrive W.F
033000050801     c                   move      orgcar        traarea
033100050801     c                   move      orgfl3        tradist
033200050801     c                   movel     tradass       traanno
033300151116     c* fornitore ragione sociale estesa
033400151116     C                   exsr      RagSocCont
033500151116     c                   eval      trasocl = RagSocKsc
033600151116      *
033700050801     c                   write     waitra00
033800121031      * scrive secondo file completo di decodifiche e campi in chiaro
033900121031     c                   exsr      secondoFile
034000121031     c                   write     waitra20
034100050623     c                   enddo
034200020304
034300020304     c                   endsr
034400981029      *
034500121031     c*-------------------------------------------------
034600121031     c     secondofile   begsr
034700121031     c*-------------------------------------------------
034800121031     c*decodifiche
034900121031     c*centrale satellitare
035000160112     c                   clear                   tracend
035100160112     c                   if        tracen  <> *blank
035200121031     c                   clear                   tibs02ds
035300121031     c                   movel     'C'           t02mod
035400121031     c                   movel     knsif         t02sif
035500121031     c                   movel     'CEN'         t02cod
035600160112     c                   movel     tracen        t02ke1
035700121031     c                   call      'TIBS02R'
035800121031     c                   parm                    KPJBA
035900121031     c                   parm                    TIBS02DS
036000121031     c                   if        t02err =  *blanks
036100160112     c                   movel     t02uni        tracend
036200121031     c                   endif
036300121031     c                   endif
036400121031      *vano carico
036500121031     c                   clear                   trabpod
036600121031     c                   if        trabpo <> *blank
036700121031     c                   clear                   tibs02ds
036800121031     c                   movel     'C'           t02mod
036900121031     c                   movel     knsif         t02sif
037000121031     c                   movel     'ANT'         t02cod
037100121031     c                   movel     'VAN'         t02ke1
037200121031     c                   movel     trabpo        t02ke2
037300121031     c                   call      'TIBS02R'
037400121031     c                   parm                    KPJBA
037500121031     c                   parm                    TIBS02DS
037600121031     c                   if        t02err =  *blanks
037700121031     c                   movel     t02uni        trabpod
037800121031     c                   endif
037900121031     c                   endif
038000121031      *blocco motore
038100121031     c                   clear                   traantd
038200121031     c                   if        traant <> *blank
038300121031     c                   clear                   tibs02ds
038400121031     c                   movel     'C'           t02mod
038500121031     c                   movel     knsif         t02sif
038600121031     c                   movel     'ANT'         t02cod
038700121105     c                   movel     'MOT'         t02ke1
038800121031     c                   movel     traant        t02ke2
038900121031     c                   call      'TIBS02R'
039000121031     c                   parm                    KPJBA
039100121031     c                   parm                    TIBS02DS
039200121031     c                   if        t02err =  *blanks
039300121031     c                   movel     t02uni        traantd
039400121031     c                   endif
039500121031     c                   endif
039600121031      *turnover
039700121031     c                   clear                   traturd
039800121031     c                   if        tratur <> *blank
039900121031     c                   clear                   tibs02ds
040000121031     c                   movel     'C'           t02mod
040100121031     c                   movel     knsif         t02sif
040200121031     c                   movel     'ATU'         t02cod
040300121031     c                   movel     tratur        t02ke1
040400121031     c                   call      'TIBS02R'
040500121031     c                   parm                    KPJBA
040600121031     c                   parm                    TIBS02DS
040700121031     c                   if        t02err =  *blanks
040800121031     c                   movel     t02uni        datu
040900121031     c                   movel     §AtuDES       traturd
041000121031     c                   endif
041100121031     c                   endif
041200121031      *allestimento
041300121031     c                   clear                   trafurd
041400121031     c                   if        trafur <> *blank
041500121031     c                   clear                   tibs02ds
041600121031     c                   movel     'C'           t02mod
041700121031     c                   movel     knsif         t02sif
041800121031     c                   movel     'ALL'         t02cod
041900121031     c                   movel     trafur        t02ke1
042000121031     c                   call      'TIBS02R'
042100121031     c                   parm                    KPJBA
042200121031     c                   parm                    TIBS02DS
042300121031     c                   if        t02err =  *blanks
042400121031     c                   movel     t02uni        trafurd
042500121031     c                   endif
042600121031     c                   endif
042700121031      *decodifica classe omologazione
042800121031     c                   clear                   traff1d
042900121031     c                   if        traff1 <> *blank
043000121031     c                   clear                   tibs02ds
043100121031     c                   movel     'C'           t02mod
043200121031     c                   movel     knsif         t02sif
043300121031     c                   movel     'OMO'         t02cod
043400121031     c                   movel     traff1        t02ke1
043500121031     c                   call      'TIBS02R'
043600121031     c                   parm                    KPJBA
043700121031     c                   parm                    TIBS02DS
043800121031     c                   if        t02err =  *blanks
043900121031     c                   movel     t02uni        traff1d
044000121031     c                   endif
044100121031     c                   endif
044200121031      *decodifica tipo alimentazione
044300121031     c                   clear                   traff2d
044400121031     c                   if        traff2 <> *blank
044500121031     c                   clear                   tibs02ds
044600121031     c                   movel     'C'           t02mod
044700121031     c                   movel     knsif         t02sif
044800121031     c                   movel     'ALI'         t02cod
044900121031     c                   movel     traff2        t02ke1
045000121031     c                   call      'TIBS02R'
045100121031     c                   parm                    KPJBA
045200121031     c                   parm                    TIBS02DS
045300121031     c                   if        t02err =  *blanks
045400121031     c                   movel     t02uni        traff2d
045500121031     c                   endif
045600121031     c                   endif
045700160113      *decodifica CCNL
045800160113     c                   clear                   traCNLd
045900160113     c                   if        traCNL <> *blank
046000160113     c                   clear                   tibs02ds
046100160113     c                   movel     'C'           t02mod
046200160113     c                   movel     knsif         t02sif
046300160113     c                   movel     'CNL'         t02cod
046400160113     c                   movel     traCNL        t02ke1
046500160113     c                   call      'TIBS02R'
046600160113     c                   parm                    KPJBA
046700160113     c                   parm                    TIBS02DS
046800160113     c                   if        t02err =  *blanks
046900160113     c                   movel     t02uni        traCNLd
047000160113     c                   endif
047100160113     c                   endif
047200160113      *decodifica QUALIFICA
047300160113     c                   clear                   traQUAd
047400160113     c                   if        traQUA <> *blank
047500160113     c                   clear                   tibs02ds
047600160113     c                   movel     'C'           t02mod
047700160113     c                   movel     knsif         t02sif
047800160113     c                   movel     'QUA'         t02cod
047900160113     c                   movel     traQUA        t02ke1
048000160113     c                   call      'TIBS02R'
048100160113     c                   parm                    KPJBA
048200160113     c                   parm                    TIBS02DS
048300160113     c                   if        t02err =  *blanks
048400160113     c                   movel     t02uni        traQUAd
048500160113     c                   endif
048600160113     c                   endif
048700160113      *decodifica LIVELLO
048800160113     c                   clear                   traLIQd
048900160113     c                   if        traLIQ <> *blank
049000160113     c                   clear                   tibs02ds
049100160113     c                   movel     'C'           t02mod
049200160113     c                   movel     knsif         t02sif
049300160113     c                   movel     'LIQ'         t02cod
049400160113     c                   movel     traLIQ        t02ke1
049500160113     c                   call      'TIBS02R'
049600160113     c                   parm                    KPJBA
049700160113     c                   parm                    TIBS02DS
049800160113     c                   if        t02err =  *blanks
049900160113     c                   movel     t02uni        traLIQd
050000160113     c                   endif
050100160113     c                   endif
050200121031      *des filiale
050300121031     c                   clear                   trafild
050400121031     c     trafil        chain     azorg01l
050500121031     c                   movel     orgdes        trafild
050600121031      *
050700121031      * descrizione area
050800121031     c                   clear                   traaread
050900121031     c                   movel     traarea       kkey
051000121031     c                   move      '05'          kcod
051100121031     c     ktab          chain     tabel00f
051200121031     c                   if        %found(tabel00f)
051300121031     c                   movel     tbluni        traaread
051400121031     c                   endif
051500121031     c*
051600121031     c                   clear                   tradistd
051700121031     c                   movel(p)  tradist       kkey
051800121031     c                   move      '17'          kcod
051900121031     c     ktab          chain     tabel00f
052000121031     c                   if        %found(tabel00f)
052100121031     c                   movel     tbluni        tradistd
052200121031     c                   endif
052300160113     c*
052400121031     c                   endsr
052500151116      * ?___________________________________________________________________
052600151116     C     RagSocCont    BegSR
052700151116      * ?___________________________________________________________________
052800151116      *
052900151116     C                   clear                   trmz70s1ds
053000151116     C                   movel(p)  TRaivas       PartitaIVA                     Input
053100151116     C                   movel(p)  'F'           SottoNatur                     Input "F/C"
053200151116     C                   movel(p)  TRasocg       Societa                        Input/Output
053300151116     C                   movel(p)  TRaFIL        Unita                          Input/Output
053400151116      *
053500151116     c                   call      'TRMZ70SR1'
053600151116     C                   parm                    trmz70s1ds
053700151116     c*
053800151116     C                   ENDSR
053900050802     c*-------------------------------------------------
054000050802     c     waitra        begsr
054100050802     c*-------------------------------------------------
054200070530     c                   clear                   totmzfil
054300070530     c                   clear                   totmzarea
054400070530     c                   clear                   totmzdis
054500070530     c                   clear                   totmzaz
054600070530     c                   clear                   primo
054700070530     c     *loval        setll     waitra0f
054800070530     c                   do        *hival
054900070530     c                   read      waitra0f                               98
055000070530     c   98              leave
055100070530      * filtro area (solo x sede)
055200070530     c                   if        varea <> *blank
055300070530     c                   move      traarea       traareaa          3
055400070530     c                   if        traareaa <> varea
055500070530     c                   iter
055600070530     c                   endif
055700070530     c                   endif
055800070530      * filtro distretto (solo x sede)
055900070530     c                   if        vdist <> *blank and tradist <> vdist
056000070530     c                   iter
056100070530     c                   endif
056200070530      *
056300070530     c                   if        primo = *blank
056400070530     c                   move      trafil        exfil
056500070530     c                   move      traarea       excar
056600070530     c                   move      tradist       exfl3
056700070530     c                   move      'X'           primo
056800070530     c                   endif
056900070530      * rottura filiale
057000070530     c                   if        trafil  <> exfil
057100091216     c                   if        vstampa = 'S'
057200070530     c                   if        line > 60
057300070530     c                   except    testafil
057400070530     c                   endif
057500091216     c                   endif
057600100114      *
057700070530     c     exfil         chain     azorg01l
057800091216     c                   if        vstampa = 'S'
057900070530     c                   except    totfil
058000091216     c                   endif
058100070530     c                   clear                   totmzfil
058200070530     c                   move      trafil        exfil
058300070530     c                   endif
058400070530      *
058500070530      * rottura area
058600070530     c                   if        traarea <> excar
058700091216     c                   if        vstampa = 'S'
058800070530     c                   if        line > 60
058900070530     c                   except    testafil
059000070530     c                   endif
059100091216     c                   endif
059200091216      *
059300070530      * descrizione area
059400070530     c                   clear                   exaread20
059500070530     c                   movel     excar         kkey
059600070530     c                   move      '05'          kcod
059700070530     c     ktab          chain     tabel00f
059800070530     c                   if        %found(tabel00f)
059900070530     c                   movel     tbluni        exaread20
060000070530     c                   endif
060100091216      *
060200091216     c                   if        vstampa = 'S'
060300070530     c                   except    detarea
060400091216     c                   endif
060500070530     c                   clear                   totmzarea
060600070530     c                   move      traarea       excar
060700070530     c   66              z-add     99            line
060800070530     c                   endif
060900070530      *
061000070530      * rottura distretto
061100070530     c                   if        tradist <> exfl3
061200091216     c                   if        vstampa = 'S'
061300070530     c                   if        line > 60
061400070530     c                   except    testafil
061500070530     c                   endif
061600091216     c                   endif
061700091216      *
061800070530     c                   clear                   exdistd20
061900070530     c                   movel(p)  exfl3         kkey
062000070530     c                   move      '17'          kcod
062100070530     c     ktab          chain     tabel00f
062200070530     c                   if        %found(tabel00f)
062300070530     c                   movel     tbluni        exdistd20
062400070530     c                   endif
062500091216      *
062600091216     c                   if        vstampa = 'S'
062700091216      *
062800070530     c                   except    detdist
062900091216      *
063000091216     c                   endif
063100091216      *
063200070530     c                   clear                   totmzdis
063300070530     c                   move      tradist       exfl3
063400070530     c   66              z-add     99            line
063500070530     c                   endif
063600070530      * dettaglio  p.o
063700091216      *
063800091216     c                   if        vstampa = 'S'
063900091216      *
064000070530     c                   if        line > 60
064100070530     c                   except    testafil
064200070530     c                   endif
064300091216      *
064400091216     c                   endif
064500091216      *
064600070530     c     exfil         chain     azorg01l
064700170809     c                   clear                   tradins
064900070606      * data disaccreditamento da stampare per turnover
065000170809     c                   if        tradin > 0
065200170809     c                   move      tradin        dataiso
065300070606     c                   move      dataiso       dataeur
065400170809     c                   move      dataeur       tradins           8 0
065500070606     c                   endif
065600091216      *
065700091216     c                   if        vstampa = 'S'
065800091216      *
065900070530     c                   except    detfil
066000091216      *
066100091216     c                   end
066200091216      *
066300070530      * incremento totale mezzi per area e filiale
066400070530     c                   add       1             totmzfil
066500070530     c                   add       1             totmzarea
066600070530     c                   add       1             totmzdis
066700070530     c                   add       1             totmzaz
066800070530
066900070530     c                   enddo
067000070530
067100070530      * fine file controlli finali x stampa totali
067200091216      *
067300091216     c                   if        vstampa = 'S'
067400091216      *
067500070531     c*                  if        lisarea = 'S' and *in80 or
067600070531     c*                            lisdist = 'S' and *in81
067700070530     c                   if        line > 60
067800070530     c                   except    testafil
067900070530     c                   endif
068000091216      *
068100091216     c                   endif
068200091216      *
068300070530      * descrizione area
068400070530     c                   clear                   exaread20
068500070530     c                   movel     excar         kkey
068600070530     c                   move      '05'          kcod
068700070530     c     ktab          chain     tabel00f
068800070530     c                   if        %found(tabel00f)
068900070530     c                   movel     tbluni        exaread20
069000070530     c                   endif
069100091216      *
069200091216     c                   if        vstampa = 'S'
069300091216      *
069400070530     c                   except    totfil
069500091216      *
069600091216     c                   end
069700091216      *
069800070530      * stampo totale area finale
069900091216      *
070000091216     c                   if        vstampa = 'S'
070100091216      *
070200070530     c                   if        *in80 or *in81 or *in82 and v1cfil = 0
070300070530     c                   except    detarea
070400070530     c                   endif
070500091216      *
070600091216     c                   endif
070700091216      *
070800070530      * stampo totale distretto finale
070900070530     c                   if        *in81 or *in82 and v1cfil = 0
071000070530     c                   clear                   exdistd20
071100070530     c                   movel(p)  exfl3         kkey
071200070530     c                   move      '17'          kcod
071300070530     c     ktab          chain     tabel00f
071400070530     c                   if        %found(tabel00f)
071500070530     c                   movel     tbluni        exdistd20
071600070530     c                   endif
071700091216      *
071800091216     c                   if        vstampa = 'S'
071900091216      *
072000070530     c                   except    detdist
072100091216      *
072200091216     c                   endif
072300091216      *
072400070530     c                   endif
072500070530      * stampo totale aziendale solo se richiesta globale di sede
072600091216      *
072700091216     c                   if        vstampa = 'S'
072800091216      *
072900070530     c                   if        *in82 and v1cfil = 0
073000070530     c                   except    detaz
073100070530     c                   endif
073200091216      *
073300091216     c                   endif
073400091216      *
073500070530
073600070531     c*                  endif
073700070530     c                   endsr
073800070530      *
073900050802      *
074000031105     c*-------------------------------------------------
074100031105     c     controv1      begsr
074200031105     c*-------------------------------------------------
074300031105     c                   clear                   v1dfil
074301170901     c                   clear                   varead
074302170901     c                   clear                   vdistd
074400070606     c                   setoff                                       9697
074500051122     c                   setoff                                       959493
074600051122     c                   setoff                                       92
074700031105     c                   if        v1cfil > 0
074800051122
074900050623     c     v1cfil        chain     azorg01l
075000050623     c                   if        %found(azorg01l)
075100031105     c                   movel     orgdes        v1dfil
075200031105     c                   else
075300031105     c                   seton                                        9796
075400051122     c                   goto      endcontr
075500031105     c                   endif
075501170901     c     v1cfil        lookup    $pogn                                  70
075502170901     c                   if        not *in70
075503170901     c                   eval      $msg = 'Filiale non abilitata'
075504170901     c                   seton                                        2896
075505170901     c                   leavesr
075506170901     c                   endif
077000051122      *
077100031105     c                   endif
077200051122      * controllo congruenza selezioni a video
077300051122     c                   if        v1cfil > 0 and
077400051122     c                             (varea <> *blank or vdist <> *blank)
077500051122     c                   seton                                        9596
077600051122     c                   goto      endcontr
077700051122     c                   endif
077800051122      *
077900051122     c                   if        varea <> *blank and vdist <> *blank
078000051122     c                   seton                                        9496
078100051122     c                   goto      endcontr
078200051122     c                   endif
078300051121      * x sede parzializza area o distretto
078400051121     C     varea         ifeq      '?'
078500051121     C                   movel     '05'          kcod
078600051121     C                   clear                   kkey
078700051121     C                   call      'X§TABER'
078800051121     C                   parm                    kkut
078900051121     C                   parm                    kcod
079000051121     C                   parm                    kkey
079100051121     C                   parm                    §des             30
079200051121     c                   movel     §des          varead
079300051121     c                   movel     kkey          varea
079400051121     C*
079500051121     C                   else
079600051121     C*
079700051121     C                   if        varea <> *blank and varea <> *zero
079800051121     C                   movel     '05'          kcod
079900051121     C                   movel(p)  varea         kkey
080000051121     C     ktab          chain     tabel00f
080100051121     c                   if        %found(tabel00f) and tblflg = *blank
080200051121     C                   movel     tbluni        varead
080300051121     c                   else
080400051121     c                   movel     *all'?'       varead
080500051121     C                   end
080501170901     c     varea         lookup    $arg                                   70
080502170901     c                   if        not *in70
080503170901     c                   eval      $msg = 'Area non abilitata'
080504170901     c                   seton                                        2896
080505170901     c                   leavesr
080506170901     c                   endif
080600051121     C                   end
080700051121     C                   end
080800051121      * distretto
080900051121     C     vdist         ifeq      '?'
081000051121     C                   movel     '17'          kcod
081100051121     C                   clear                   kkey
081200051121     C                   call      'X§TABER'
081300051121     C                   parm                    kkut
081400051121     C                   parm                    kcod
081500051121     C                   parm                    kkey
081600051121     C                   parm                    §des
081700051121     c                   movel     §des          vdistd
081800051121     c                   movel     kkey          vdist
081900051121     C*
082000051121     C                   else
082100051121     C*
082200051121     C                   if        vdist <> *blank and vdist <> *zero
082300051121     C                   movel     '17'          kcod
082400051121     C                   movel(p)  vdist         kkey
082500051121     C     ktab          chain     tabel00f
082600051121     c                   if        %found(tabel00f) and tblflg = *blank
082700051121     C                   movel     tbluni        vdistd
082800051121     c                   else
082900051121     c                   movel     *all'?'       vdistd
083000051121     C                   end
083001170901     c     vdist         lookup    $dig                                   70
083002170901     c                   if        not *in70
083003170901     c                   eval      $msg = 'Distretto non abilitato'
083004170901     c                   seton                                        2896
083005170901     c                   leavesr
083006170901     c                   endif
083100051121     C                   end
083200051121     C                   end
083300070605      *turnover
083400070605     c                   setoff                                       43
083500070605     c                   if        vtratur <> *blank
083600070605     c                   if        vtrasco <> *blank or
083700070605     c                             vtratmp <> *blank or
083800070605     c                             vtrafur <> *blank or
083900070605     c                             vtrapub <> *blank or
084000070619     c                             vtrarif <> *blank or
084100070605     c                             vtraant <> *blank or
084200070605     c                             vtrabpo <> *blank or
084300070605     c                             vtraff1 <> *blank or
084400070605     c                             vtraff2 <> *blank or
084500070605     c                             vtraspi <> *blank or
084600091006     c                             vtraris <> *blank or
084700120914     c                             vtraloci <> *blank or
084800091006     c                             vtracor <> *blank
084900070605     c                   seton                                        4396
085000070605     c                   goto      endcontr
085100070605     c                   endif
085200070605     c                   endif
085300070605      *data disaccr. per turnover
085400070605     c                   setoff                                       44
085500070605     c                   if        vtradfi = 0  and vtratur <> *blank
085600070605     c                   seton                                        4496
085700070605     c                   goto      endcontr
085800070605     c                   endif
085900091216      *
086000070605     c                   setoff                                       41
086100091216      * il controllo deve essere attivo solo per utenti di filiale :
086200091216      * se immessa una data deve esserci anche il flag di TURNOVER ='S'
086300091216     c                   if        not *in82
086400091216     c                   if        vtradfi > 0  and vtratur = *blank
086500091216     c                   seton                                        4196
086600091216     c                   goto      endcontr
086700091216     c                   endif
086800091216     c                   endif
086900091216      *
087000070605     c                   if        vtradfi > 0
087100070605     C                   MOVE      Vtradfi       G02DAT
087200070605     C                   MOVEL     *BLANK        G02ERR
087300070605     C                   CALL      'XSRDA8'
087400070605     C                   PARM                    WLBDA8
087500070605     C     G02ERR        IFEQ      '1'
087600070605     C                   SETON                                        4295
087700070605     c                   goto      endcontr
087800070605     C                   END
087900070605     c                   move      g02dat        vtradfi
088000070605     c                   move      g02inv        vtradfig          8 0
088100070605     c                   endif
088200070529      *allestimento (tabella)
088300070529     c                   setoff                                       45
088400070529     c                   if        vtrafur = '?'
088500070529     c                   clear                   tibs02ds
088600070529     c                   movel     'R'           t02mod
088700070529     c                   movel     knsif         t02sif
088800070529     c                   movel     'ALL'         t02cod
088900070529     c                   call      'TIBS02R'
089000070529     c                   parm                    KPJBA
089100070529     c                   parm                    TIBS02DS
089200070529     c                   if        t02err =  *blanks
089300070529     c                   movel     t02uni        vtrafurd
089400070529     c                   movel     t02ke1        vtrafur
089500070529     c                   goto      endcontr
089600070529     c                   endif
089700070529     c                   else
089800070529     c                   if        vtrafur <> *blank
089900070529     c                   clear                   tibs02ds
090000070529     c                   movel     'C'           t02mod
090100070529     c                   movel     knsif         t02sif
090200070529     c                   movel     'ALL'         t02cod
090300070529     c                   movel     vtrafur       t02ke1
090400070529     c                   call      'TIBS02R'
090500070529     c                   parm                    KPJBA
090600070529     c                   parm                    TIBS02DS
090700070529     c                   if        t02err =  *blanks
090800070529     c                   movel     t02uni        vtrafurd
090900070529     c                   else
091000070529     c                   seton                                        4596
091100070529     c                   goto      endcontr
091200070529     c                   endif
091300070529     c                   endif
091400070529     c                   endif
091500070529      * antifurto vano carico
091600070529     c                   setoff                                       46
091700070529     c                   if        vtrabpo = '?  '
091800070529     c                   clear                   tibs02ds
091900070529     c                   movel     'R'           t02mod
092000070529     c                   movel     knsif         t02sif
092100070529     c                   movel     'ANT'         t02cod
092200070529     c                   movel(p)  'VAN'         t02ke1
092300070529     c                   call      'TIBS02R'
092400070529     c                   parm                    KPJBA
092500070529     c                   parm                    TIBS02DS
092600070529     c                   if        t02err =  *blanks
092700070529     c                   movel     t02uni        vtrabpod
092800070529     c                   movel     t02ke2        vtrabpo
092900070529     c                   endif
093000070529     c                   else
093100070529     c                   if        vtrabpo <> *blank
093200070529     c                   clear                   tibs02ds
093300070529     c                   movel     'C'           t02mod
093400070529     c                   movel     knsif         t02sif
093500070529     c                   movel     'ANT'         t02cod
093600070529     c                   movel     'VAN'         t02ke1
093700070529     c                   movel     vtrabpo       t02ke2
093800070529     c                   call      'TIBS02R'
093900070529     c                   parm                    KPJBA
094000070529     c                   parm                    TIBS02DS
094100070529     c                   if        t02err =  *blanks
094200070529     c                   movel     t02uni        vtrabpod
094300070529     c                   else
094400070529     c                   seton                                        4696
094500070529     c                   goto      endcontr
094600070529     c                   endif
094700070529     c                   endif
094800070529     c                   endif
094900070529      * antifurto blocco motore
095000070529     c                   setoff                                       47
095100070529     c                   if        vtraant = '?  '
095200070529     c                   clear                   tibs02ds
095300070529     c                   movel     'R'           t02mod
095400070529     c                   movel     knsif         t02sif
095500070529     c                   movel     'ANT'         t02cod
095600070529     c                   movel(p)  'MOT'         t02ke1
095700070529     c                   call      'TIBS02R'
095800070529     c                   parm                    KPJBA
095900070529     c                   parm                    TIBS02DS
096000070529     c                   if        t02err =  *blanks
096100070529     c                   movel     t02uni        vtraantd
096200070529     c                   movel     t02ke2        vtraant
096300070529     c                   endif
096400070529     c                   else
096500070529     c                   if        vtraant <> *blank
096600070529     c                   clear                   tibs02ds
096700070529     c                   movel     'C'           t02mod
096800070529     c                   movel     knsif         t02sif
096900070529     c                   movel     'ANT'         t02cod
097000070529     c                   movel     'MOT'         t02ke1
097100070529     c                   movel     vtraant       t02ke2
097200070529     c                   call      'TIBS02R'
097300070529     c                   parm                    KPJBA
097400070529     c                   parm                    TIBS02DS
097500070529     c                   if        t02err =  *blanks
097600070529     c                   movel     t02uni        vtraantd
097700070529     c                   else
097800070529     c                   seton                                        4796
097900070529     c                   goto      endcontr
098000070529     c                   endif
098100070529     c                   endif
098200070529     c                   endif
098300070529      *Classe omologazione (tabella)
098400070529     c                   setoff                                       31
098500070529     c                   clear                   vtraff1d
098600070529     c                   if        vtraff1 = '?'
098700070529     c                   clear                   tibs02ds
098800070529     c                   movel     'R'           t02mod
098900070529     c                   movel     knsif         t02sif
099000070529     c                   movel     'OMO'         t02cod
099100070529     c                   call      'TIBS02R'
099200070529     c                   parm                    KPJBA
099300070529     c                   parm                    TIBS02DS
099400070529     c                   if        t02err =  *blanks
099500070529     c                   movel     t02uni        vtraff1d
099600070529     c                   movel     t02ke1        vtraff1
099700070529     c                   goto      endcontr
099800070529     c                   endif
099900070529     c                   else
100000070529     c                   if        vtraff1 <> *blank
100100070529     c                   clear                   tibs02ds
100200070529     c                   movel     'C'           t02mod
100300070529     c                   movel     knsif         t02sif
100400070529     c                   movel     'OMO'         t02cod
100500070529     c                   movel     vtraff1       t02ke1
100600070529     c                   call      'TIBS02R'
100700070529     c                   parm                    KPJBA
100800070529     c                   parm                    TIBS02DS
100900070529     c                   if        t02err =  *blanks
101000070529     c                   movel     t02uni        vtraff1d
101100070529     c                   else
101200070529     c                   seton                                        3195
101300070529     c                   goto      endcontr
101400070529     c                   endif
101500070529     c                   endif
101600070529     c                   endif
101700070529      *tipo alimentazione (tabella)
101800070529     c                   setoff                                       32
101900070529     c                   clear                   vtraff2d
102000070529     c                   if        vtraff2 = '?'
102100070529     c                   clear                   tibs02ds
102200070529     c                   movel     'R'           t02mod
102300070529     c                   movel     knsif         t02sif
102400070529     c                   movel     'ALI'         t02cod
102500070529     c                   call      'TIBS02R'
102600070529     c                   parm                    KPJBA
102700070529     c                   parm                    TIBS02DS
102800070529     c                   if        t02err =  *blanks
102900070529     c                   movel     t02uni        vtraff2d
103000070529     c                   movel     t02ke1        vtraff2
103100070529     c                   goto      endcontr
103200070529     c                   endif
103300070529     c                   else
103400070529     c                   if        vtraff2 <> *blank
103500070529     c                   clear                   tibs02ds
103600070529     c                   movel     'C'           t02mod
103700070529     c                   movel     knsif         t02sif
103800070529     c                   movel     'ALI'         t02cod
103900070529     c                   movel     vtraff2       t02ke1
104000070529     c                   call      'TIBS02R'
104100070529     c                   parm                    KPJBA
104200070529     c                   parm                    TIBS02DS
104300070529     c                   if        t02err =  *blanks
104400070529     c                   movel     t02uni        vtraff2d
104500070529     c                   else
104600070529     c                   seton                                        3295
104700070529     c                   goto      endcontr
104800070529     c                   endif
104900070529     c                   endif
105000070529     c                   endif
105100051121
105200051122     c     endcontr      endsr
105300050802     c*-------------------------------------------------
105400050802     c     comandi       begsr
105500050802     c*-------------------------------------------------
105600050802     C                   close     waitra0f
105700050802      *
105800070608     C                   z-add     99            luncmd
105900050802     C                   CALL      'QCMDEXC'
106000050802     C                   parm                    coman
106100050802     C                   parm                    luncmd
106200050802      *
106300070601     C  nkc              OPEN      waitra0f
106400050802     c                   endsr
106500070608     c*-------------------------------------------------
106600070608     c     comandi2      begsr
106700070608     c*-------------------------------------------------
106800070608      *
106900070608     C                   z-add     99            luncmd
107000070608     C                   CALL      'QCMDEXC'                            34
107100070608     C                   parm                    coman
107200070608     C                   parm                    luncmd
107300070608      *
107400070608     c                   endsr
107500121002     c*-------------------------------------------------
107600121002     c     comandiB      begsr
107700121002     c*-------------------------------------------------
107800121105     C                   close     waitra2f
107900121002      *
108000121002     C                   z-add     99            luncmd
108100121002     C                   CALL      'QCMDEXC'
108200121002     C                   parm                    coman
108300121002     C                   parm                    luncmd
108400121002      *
108500121105     C  nkc              OPEN      waitra2f
108600121002     c                   endsr
108700011026     c*-------------------------------------------------
108800011026     c     *inzsr        begsr
108900011026     c*-------------------------------------------------
109000020304     c     *entry        plist
109100020304     c                   parm                    kpjba
109200050801
109300050801     c     ktab          klist
109400050801     c                   kfld                    kkut              1 0
109500050801     c                   kfld                    kcod              2
109600050801     c                   kfld                    kkey              8
109700050801     c                   z-add     1             kkut
109800050801     c                   move      '05'          kcod
109900050801     c
110000050801
110100020218     C                   time                    w0120            14 0
110200020218     C                   move      w0120         wdat              8 0
110300020404     c                   z-add     99            line
110301170901     c                   exsr      repdatiute
110302170901      * verifica se profili di filiale con autorizzazione a area o distretto
110303170901      * oppure se profilo di sede nel caso abilita selezione area e distretto
110304170901     c                   if        wabi = 'AZ' or wabi = 'DI'
110305170901     c                   seton                                        81
110306170901     c                   endif
110307170901     c                   if        wabi = 'RA'
110308170901     c                   seton                                        80
110309170901     c                   endif
110310170901     c                   if        dutpou <> 046
110311170901     c                   move      dutpou        v1cfil
110312170901     c                   if        wabi <>'PO'
110313170901     c                   seton                                        82
110314170901     c                   endif
110315170901     c                   endif
110900051017      *se profilo di filiale per pilotare le letture solo sul P.O
111000050802     c                   clear                   lisarea
111100050802     c                   clear                   lisdist
111200080502     c                   setoff                                       84
111300051017     c                   eval      saltp = 'N'
111400080502      *
111500051017      * x filiale duplica file di work in qtemp
111600051017     c                   movea     cmd2          coman
111700051017     c                   exsr      comandi
111800121002     c                   movea     cmd8          coman
111900121002     c                   exsr      comandib
112000080502      *
114300050802
114400050805     c                   if        v1cfil <> 0
114500031105     c     v1cfil        chain     azorg01l
114600031105     c                   if        %found(azorg01l)
114700031105     c                   movel     orgdes        v1dfil
114800050802     c                   move      orgcar        warea
114900050802     c                   move      orgfl3        wdist
115000031105     c                   endif
115100050805     c                   endif
115200070608      * creo nome file per dati utente in libreria di wrk
115300070608     c                   eval      file = 'AU' + knmus
115400121105     c                   eval      file2 = 'AW' + knmus
115500011026     c                   endsr
115501170901     C*--------------------------------------------------------------------------------------------*
115502170901     C* REPERISCE I DATI UTENTE
115503170901     C*--------------------------------------------------------------------------------------------*
115504170901     C     REPDATIUTE    BEGSR
115505170901     C*
115506170901     C* INIZIALIZZA VARIABILI DI WRK
115507170901     C                   CLEAR                   TIBS34DS
115508170901     C                   CLEAR                   AZUTEDS
115509170901     C                   CLEAR                   DDATIUTE
115510170901     C*
115511170901     C     *DTAARA       DEFINE    §azute        azuteds
115512170901     C     *DTAARA       DEFINE    §datiute      ddatiute
115513170901     C                   IN(E)     *DTAARA
115514170901     C                   IF        %Error
115515170901     C                   EVAL      I34Tla = 'L'
115516170901     C                   CALL      'TIBS34R'
115517170901     C                   PARM                    Tibs34Ds
115518170901     C                   IN        *DTAARA
115519170901     C                   ENDIF
115520170901     c                   Clear                   wabi
115521170901     c                   Clear                   dLat
115522170901
115523170901      * Verifica errori e autorità profilo
115524170901s   1c                   Select
115525170901      * se ho errori nei dati utente esco dal pgm
115526170901w   1c                   When      DutErr = 'E'
115527170901     c                   seton                                        lr
115528170901     c                   return
115529170901      * se non c'è l'abilitazione
115530170901      * --> se 1° livello, abilitazioni al terminal
115531170901      *     se 2° livello, abilitazioni al punto operativo
115532170901w   1c                   When      UteAut = *Blanks
115533170901if  2c                   If        DutLpo = '1'
115534170901     c                   Eval      wabi   = 'TP'
115535170901e   2c                   EndIf
115536170901if  2c                   If        DutLpo = '2'
115537170901     c                   Eval      wabi   = 'PO'
115538170901e   2c                   EndIf
115539170901if  2c                   If        DutLpo = 'S'
115540170901     c                   Eval      wabi   = 'AZ'
115541170901e   2c                   EndIf
115542170901      * carica le abilitazioni del profilo
115543170901x   1c                   Other
115544170901     c                   Movel     UteFaf        Dute01
115545170901     c                   If        §Utecerv <> *Blanks
115548170901     c                   Eval      wabi = §utecerv
115549170901     c                   EndIf
115550170901e   1c                   EndSl
115551170901      * controllo se ok l'abilitazione dell'utente
115552170901     c                   Clear                   Tibs02ds
115553170901     c                   Eval      T02Mod = 'C'
115554170901     c                   Eval      T02Sif = knsif
115555170901     c                   Eval      T02Cod = 'LAT'
115556170901     c                   Movel(p)  wabi          T02Ke1
115557170901     c                   Call      'TIBS02R'
115558170901     c                   Parm                    kpjba
115559170901     c                   Parm                    Tibs02ds
115560170901if  1c                   If        T02Err = *Blanks
115561170901     c                   Eval      dLat = T02Uni
115562170901e   1c                   EndIf
115563170901      * errore o non abilitato (impossibile xchè controllato nel filtro)
115564170901if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S' or
115565170901     c                             wabi = 'NO' or wabi = *blank
115566170901     c                   seton                                        98
115567170901     c                   call      'XUTENOAB'
115568170901     c                   parm                    kpjba
115569170901     c                   seton                                        lr
115570170901     c                   return
115571170901   x1c                   endif
115572170901      * Reperimento dei P.O. gestibili dall'utente
115573170901     c                   clear                   TRUL31DS
115574170901     c                   clear                   TRUL31DS2
115575170901     c                   eval      I31abi = wabi
115576170901     c                   eval      I31cdi = DUTdis
115577170901     c                   eval      I31car = DUTare
115578170901     c                   eval      I31cpo = DUTpou
115579170901     c                   call      'TRUL31R'
115580170901     c                   parm                    KPJBA
115581170901     c                   parm                    TRUL31DS
115582170901     c                   parm                    TRUL31DS2
115583170901if  2c                   if        O31pog > *zeros
115584170901     c                   movea     O31pog        $pog
115585170901     c                   movea     O31arg        $arg
115586170901     c                   movea     O31dig        $dig
115587170901x   2c                   else
115588170901     c* impossibile perchè controllato nel filtrp
115589170901     c                   seton                                        lr
115590170901     c                   return
115591170901    1c                   endif
115592170901     c                   do        250           g                 3 0
115593170901     c                   if        $pog(g) > *zero
115594170901     c                   move      $pog(g)       $pogn(g)
115595170901     c                   else
115596170901     c                   move      999           $pogn(g)
115597170901     c                   endif
115598170901     c                   enddo
115599170901     c                   endsr
115600050802      *---------------------------------------------------------
115700170808     Oprtf198   E            legenda        1 02
115800090326     o                                           10 'TRMZ74R'
115900070531     o                       knmus               22
116000070606     o                                        +   1 'Lista Automezzi/Autisti -
116100070606     o                                              di città  PARAMETRI'
116200070531     o                       udate         y  +   3
116300070531     O                                        +   1 'Pag.'
116400070531     O                       page          z  +   1
116500070531     O          E            legenda     2
116600070531     o                                            8 'Filiale'
116700070531     o                       v1cfil           +   2
116800070531     o                       v1dfil           +   1
116900070531     o                                        +   2 'Tutta l''area'
117000070531     o                       lisarea          +   1
117100070531     o                                        +   2 'Tutto distretto'
117200070531     o                       lisdist          +   1
117300070531     o                                        +   2 'Solo area'
117400070531     o                       varea            +   1
117500070531     o                       varead           +   1
117600070531     o                                        +   2 'Solo distr.'
117700070531     o                       vdist            +   1
117800070531     o                       vdistd           +   1
117900070531     O          E            legenda     0
118000070531     o                                            8 'Filiale'
118100070531     o                       v1cfil           +   2
118200070531     o                       v1dfil           +   1
118300070531     o                                        +   2 'Tutta l''area'
118400070531     o                       lisarea          +   1
118500070531     o                                        +   2 'Tutto distretto'
118600070531     o                       lisdist          +   1
118700070531     o                                        +   2 'Solo area'
118800070531     o                       varea            +   1
118900070531     o                       varead           +   1
119000070531     o                                        +   2 'Solo distr.'
119100070531     o                       vdist            +   1
119200070531     o                       vdistd           +   1
119300070531     O          E            legenda     0
119400070531     o                                            8 'Filiale'
119500070531     o                       v1cfil           +   2
119600070531     o                       v1dfil           +   1
119700070531     o                                        +   2 'Tutta l''area'
119800070531     o                       lisarea          +   1
119900070531     o                                        +   2 'Tutto distretto'
120000070531     o                       lisdist          +   1
120100070531     o                                        +   2 'Solo area'
120200070531     o                       varea            +   1
120300070531     o                       varead           +   1
120400070531     o                                        +   2 'Solo distr.'
120500070531     o                       vdist            +   1
120600070531     o                       vdistd           +   1
120700081204     O          E            legenda     2
120800081204     o                                           50 'Tipo Autista ..........'
120900081204     o                       vtpaut           +   2
121000070605     O          E            legenda     2
121100070605     o                                           50 'Turnover...............'
121200070605     o                       vtratur          +   2
121300070605     o                                        +   2 'Dalla data'
121400070605     o                       vtradfi          +   2 '  /  /    '
121500070531     O          E            legenda     2
121600170221     o                                           50 'Senza automezzo .......'
121700070531     o                       vtraris          +   2
121800100114     O          E            legenda     2
121900091006     o                                           50 'A perdere .............'
122000091006     o                       vtracor          +   2
122100070531     O          E            legenda     2
122200070531     o                                           50 'Mezzo di scorta........'
122300070531     o                       vtrasco          +   2
122400120914     O          E            legenda     2
122500170714     o                                           51 'Centro St/Loc.Turistiche'
122600170714     o                       vtraloci         +   1
122700170626     O          E            legenda     2
122800170714     o                                           50 'Picco Lavorazione......'
122900170626     o                       vtraatp          +   2
123000070531     O          E            legenda     2
123100070531     o                                           50 'Colore automezzo.......'
123200070531     o                       vtratmp          +   2
123300070605     O          E            legenda     2
123400070605     o                                           50 'Pubblicità.............'
123500070605     o                       vtrapub          +   2
123600070619     O          E            legenda     2
123700070619     o                                           50 'Rifrangente............'
123800070619     o                       vtrarif          +   2
123900070531     O          E            legenda     2
124000070531     o                                           50 'Sponda idraulica.......'
124100070531     o                       vtraspi          +   2
124200070531     O          E            legenda     2
124300070531     o                                           50 'Allestimento ..........'
124400070531     o                       vtrafur          +   2
124500070531     o                       vtrafurd         +   2
124600070531     O          E            legenda     2
124700070531     o                                           50 'Classe omologazione....'
124800070531     o                       vtraff1          +   2
124900070531     o                       vtraff1d         +   2
125000070531     O          E            legenda     2
125100070531     o                                           50 'Tipo alimentazione.....'
125200070531     o                       vtraff2          +   2
125300070531     o                       vtraff2d         +   2
125400070531     O          E            legenda     2
125500070531     o                                           50 'Antifurto blocca porte.'
125600070531     o                       vtrabpo          +   2
125700070531     o                       vtrabpod         +   2
125800070531     O          E            legenda     2
125900070531     o                                           50 'Antifurto blocco motore'
126000070531     o                       vtraant          +   2
126100070531     o                       vtraantd         +   2
126200070531      *----------------------------------------------------------
126300070531     O          E            testafil       1 02
126400090326     o                                           10 'TRMZ74R'
126500050802     o                       knmus               22
126600070606     o                                        +   1 'Lista Automezzi/Autisti -
126700070606     o                                              di città ELENCO  '
126800050802     o                       udate         y  +   3
126900050802     O                                        +   1 'Pag.'
127000050802     O                       page          z  +   1
127100050803     O          E            testafil    2  1
127200050803     o                                           16 'Societa'
127300050803     o                                           42 'Autista'
127400050803     o                                           66 'Mezzo'
127500050803     o                                           84 'Targa'
127600170809     o                                          102 'Data Pre Acc.'
127601170808     o                                          110 'Tp.All'
127602170808     o                                          118 'Sp.idr'
127603170808     o                                          126 'Tp.Ant'
127604170808     o                                          134 'Bl.Por'
127605170808     o                                          142 'Colore'
127606170808     o                                          150 'Pubb.'
127700170809     O          E            detfil      2
127800051122     o                       TRASOC           +   9
127900050802     o                       TRAAUT           +   1
128000050802     o                       TRATIA           +   1
128100050802     o                       TRASCO           +   1
128200050802     o                       TRATAA           +   1
128300170809     o                       TRAdins            100 '  /  /    '
128302170808     o                       TRAFUR             108
128303170808     o                       TRASPI             116
128304170808     o                       TRAANT             124
128305170808     o                       TRABPO             132
128306170808     o                       TRATMP             140
128307170808     o                       TRAPUB             148
128400050803     O          E            totfil      1  1
128500050803     o                                              'Totale P.O '
128600050803     o                       exfil            +   1
128700051121     o                       orgdes           +   1
128800051122     o                       totmzfil      z  +   4
128900050803     O          E            detarea     1  1
129000050803     o                                              'Totale area'
129100050803     o                       excar            +   1
129200051121     o                       exaread20        +   1
129300051122     o                       totmzarea     z  +   4
129400050803     O          E            detdist     1
129500050803     o                                              'Totale distretto'
129600050803     o                       exfl3            +   1
129700051121     o                       exdistd20        +   1
129800051121     o                       totmzdis      z  +   1
129900051018     O          E            detaz       1
130000051122     o                                              'Totale azienda'
130100051122     o                       totmzaz       z     45
130200050802**  CMD1
130300050802CLRPFM    FILE(WAITRA0F)
130400050802**  CMD2
130500050802CRTDUPOBJ OBJ(WAITRA0F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
130600050802**  CMD3
130700050802DLTF FILE(QTEMP/WAITRA0F)
130800070608**  CMD4
130900160115DLTF   FILE(WRKQUERY/xxxxxxxxxx)
131000070608**  CMD5
131100070608CRTDUPOBJ OBJ(WAITRA0F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(xxxxxxxxxx) DATA(*YES)
131200121002**  CMD7
131300121105CLRPFM    FILE(WAITRA2F)
131400121002**  CMD8
131500121105CRTDUPOBJ OBJ(WAITRA2F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(QTEMP)
131600121002**  CMD9
131700121105DLTF FILE(QTEMP/WAITRA2F)
131800121002**  CMD11
131900121105CRTDUPOBJ OBJ(WAITRA2F) FROMLIB(*LIBL) OBJTYPE(*FILE) TOLIB(WRKQUERY) NEWOBJ(xxxxxxxxxx) DATA(*YES)
