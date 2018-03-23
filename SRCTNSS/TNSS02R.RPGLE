000100071228     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200071228
000300071228      //-----------------------------------------------------------------------
000400071228      //? STATISTICA UTILIZZO PARTICOLARITA
000500071228      //-----------------------------------------------------------------------
000600071228
000700071228     fazorg01l  if   e           k disk
000800071228     ftabel00f  if   e           k disk
000900071228     ftitas38c  if   e           k disk
001000100729     ftntam01l  if   e           k disk
001100071228     fwfupa01l  uf a e           k disk    usropn
001200071228     fqsysprt   o    f  132        printer oflind(*inof)
001300071228
001400071228      //-----------------------------------------------------------------------
001500071228      //? VARIABILI
001600071228      //-----------------------------------------------------------------------
001700071228     d codut           s              1  0 inz(1)
001800071228     d lengh           s             15  5 inz(80)
001900071228     d kcod            s                   like(tblcod)
002000071228     d kkey            s                   like(tblkey)
002100080103     d pdataal         s             10
002200080103     d pdatada         s             10
002300071228     d xx              s              3  0 inz
002400100729     d nn              s              3  0 inz
002500100729     d YY              s              3  0 inz
002600100729     d spar            s              2    dim(100)
002700100729     d wksc            s                   like(upaksc)
002800071228     d wdataspe        s                   like(upadtada)
002900071228     d wdate           s                   like(upadtada)
003000071228     d wpar            s                   like(upacpar)
003100071228     d wtblkey         s                   like(tblkey)
003200071228     d wtime           s             14  0
003300071228     d $recok          s               n
003400100729     D Ptr             S               *
003500100729     D                                     INZ(%ADDR(§7UCPA))
003600100729     D PAR7U           S                   LIKE(WPAR)
003700100729     D                                     DIM(30)
003800100729     D                                     BASED(Ptr)
003900071228
004000071228      //-----------------------------------------------------------------------
004100071228      //? SCHIERE
004200071228      //-----------------------------------------------------------------------
004300071228     d cmd             s             80    dim(1) ctdata perrcd(1)
004400071228     d stb             s              2    dim(30)
004500071228     d stp             s              1    dim(30)
004600071228
004700071228      //-----------------------------------------------------------------------
004800071228      //? STRUTTURE DATI
004900071228      //-----------------------------------------------------------------------
005000071228     d param           ds
005100071228     d  waada                  1      4  0
005200071228     d  wmgda                  5      8  0
005300071228     d  wmmda                  5      6  0
005400071228     d  wggda                  7      8  0
005500071228     d  wdatada                1      8  0
005600071228     d  waaal                  9     12  0
005700071228     d  wmmal                 13     14  0
005800071228     d  wggal                 15     16  0
005900071228     d  wdataal                9     16  0
006000071228
006100071228     d tasdsp          ds
006200071228     d  tasaas
006300071228     d  tasmgs
006400071228
006500071228     d dstb          e ds
006600071228     d ds7q          e ds
006700071228     d ds7r          e ds
006800071228     d ds7s          e ds
006900071228     d ds7v          e ds
007000100729     d ds7U          e ds
007100071228     d ds_cnaco      e ds                  inz extname(cnaco00f)
007200071228     d ds_cnind      e ds                  inz extname(cnind00f)
007300071228     d ds_cnclp      e ds                  inz extname(cnclp00f)
007400071228     d ds_fncls      e ds                  inz extname(fncls00f)
007500071228     d kpjba         e ds
007600071228     d tibs10ds      e ds
007700071228     d tibs69ds      e ds
007800071228
007900071228      //-----------------------------------------------------------------------
008000071228
008100071228      * clrpfm del file di work
008200071228     c                   call(e)   'QCMDEXC'
008300071228     c                   parm                    cmd(1)
008400071228     c                   parm                    lengh
008500071228
008600071228      * se riuscito comando vado avanti con l'elaborazione
008700071228     c                   if        not %error
008800071228
008900071228      * apro il file di work
009000071228     c                   exsr      sr_openfile
009100071228      * stampo inizio elaborazione
009200071228     c                   exsr      sr_stptesta
009300071228      * elaboro le bolle
009400071228     c                   exsr      sr_elabora
009500071228      * aggiungo i clienti abilitati alle particolarità ma senza bolle
009600071228     c                   exsr      sr_elab7u
009700071228      * stampo fine elaborazione
009800071228     c                   exsr      sr_stpfine
009900071228     c                   endif
010000071228
010100071228     c                   eval      *inlr = *on
010200071228
010300071228      //-----------------------------------------------------------------------
010400071228      //? Apro il file di work
010500071228      //-----------------------------------------------------------------------
010600071228     c     sr_openfile   begsr
010700071228
010800071228     c                   open      wfupa01l
010900071228
011000071228     c                   endsr
011100071228
011200071228      //-----------------------------------------------------------------------
011300071228      //? Stampa testata
011400071228      //-----------------------------------------------------------------------
011500071228     c     sr_stptesta   begsr
011600071228
011700080103     c                   eval      pdatada = %editc(wggda:'X') + '/' +
011800080103     c                                       %editc(wmmda:'X') + '/' +
011900080103     c                                       %editc(waada:'X')
012000080103     c                   eval      pdataal = %editc(wggal:'X') + '/' +
012100080103     c                                       %editc(wmmal:'X') + '/' +
012200080103     c                                       %editc(waaal:'X')
012300071228     c                   except    testa
012400071228
012500071228     c                   endsr
012600071228
012700071228      //-----------------------------------------------------------------------
012800071228      //? Elaborazione titas
012900071228      //-----------------------------------------------------------------------
013000071228     c     sr_elabora    begsr
013100071228
013200071228     c     ktitas        setll     titas38c
013300071228     c                   do        *hival
013400071228     c                   read      titas38c
013500071228
013600071228if  2c                   if        not %eof(titas38c)
013700071228     c                   eval      wdataspe = %dec(tasdsp:8:0)
013800071228e   2c                   endif
013900071228      * fine file esco
014000071228     c                   if        %eof(titas38c) or
014100071228      * leggo fino alla data 'al'
014200071228     c                             wdataspe > wdataal
014300071228     c                   leave
014400071228     c                   endif
014500071228
014600071228      * controllo se record bolla ok
014700071228     c                   exsr      sr_recok
014800071228     c                   if        not $recok
014900071228     c                   iter
015000071228     c                   endif
015100071228
015200071228      * se bolla in porto assegnato prendo il tasccm se presente
015300071228     c                   eval      wksc = tasksc
015400071228     c                   if        stp(xx) = 'A' and tasccm > *zeros
015500071228     c                   eval      wksc = tasccm
015600071228     c                   endif
015700100729     c* aggancio tariffe
015800100729     c                   exsr      sr_partar
015900071228
016000071228      * verifico se cliente + particolarità è già presente sul file
016100071228      * --> giacenza
016200071228     c                   if        tasgga <> *blanks
016300071228     c                   eval      wpar = tasgga
016400071228     c     kwfupa        chain     wfupa01l
016500071228     c                   if        %found(wfupa01l)
016600071228      *     - è presente sommo colli e volume
016700071228     c                   exsr      sr_aggiorna
016800071228      *     - non è presente
016900071228     c                   else
017000071228     c                   clear                   wfupa000
017100071228      *       recupero i dati necessari da memorizzare sul file
017200071228     c                   exsr      sr_daticom
017300071228     c                   exsr      sr_datigga
017400071228      *       scrivo nuovo record
017500071228     c                   exsr      sr_scrivi
017600071228     c                   endif
017700071228     c                   endif
017800071228      * --> consegna
017900071228     c                   if        tasgma <> *blanks
018000071228     c                   eval      wpar = tasgma
018100071228     c     kwfupa        chain     wfupa01l
018200071228     c                   if        %found(wfupa01l)
018300071228      *     - è presente sommo colli e volume
018400071228     c                   exsr      sr_aggiorna
018500071228      *     - non è presente
018600071228     c                   else
018700071228     c                   clear                   wfupa000
018800071228      *       recupero i dati necessari da memorizzare sul file
018900071228     c                   exsr      sr_daticom
019000071228     c                   exsr      sr_datigma
019100071228      *       scrivo nuovo record
019200071228     c                   exsr      sr_scrivi
019300071228     c                   endif
019400071228     c                   endif
019500071228      * --> particolarità varie
019600071228     c                   if        %subst(tasgva:1:1) <> *blanks
019700071228     c                   eval      wpar = %subst(tasgva:1:1)
019800071228     c     kwfupa        chain     wfupa01l
019900071228     c                   if        %found(wfupa01l)
020000071228      *     - è presente sommo colli e volume
020100071228     c                   exsr      sr_aggiorna
020200071228      *     - non è presente
020300071228     c                   else
020400071228     c                   clear                   wfupa000
020500071228      *       recupero i dati necessari da memorizzare sul file
020600071228     c                   exsr      sr_daticom
020700071228     c                   exsr      sr_datigva
020800071228      *       scrivo nuovo record
020900071228     c                   exsr      sr_scrivi
021000071228     c                   endif
021100071228     c                   endif
021200071228
021300071228     c                   enddo
021400071228
021500071228     c                   endsr
021600071228
021700071228      //-----------------------------------------------------------------------
021800071228      //? Elaborazione clienti abilitati alle particolarità
021900071228      //-----------------------------------------------------------------------
022000071228     c     sr_elab7u     begsr
022100071228
022200071228     c                   eval      kcod = '7U'
022300071228     c     ktabel01      setll     tabel00f
022400071228     c                   do        *hival
022500071228     c     ktabel01      reade     tabel00f
022600071228      * fine file esco
022700071228     c                   if        %eof(tabel00f)
022800071228     c                   leave
022900071228     c                   endif
023000071228      * annullato leggo sucessivo
023100071228     c                   if        tblflg <> *blanks
023200071228     c                   iter
023300071228     c                   endif
023400071228
023500071228     c                   eval      wtblkey = tblkey
023600071228     c                   eval      wksc = %dec(%subst(wtblkey:1:7):7:0)
023700100729      * controllo se cliente + particolarità già presente sul file di work
023800071228     c                   if        %subst(tbluni:1:2) > *blanks
023900100729     c* aggancio tariffe
024000100729     c                   exsr      sr_partar
024100100729     C                   MOVEL     TBLUNI        DS7U
024200100729     C                   DO        30            YY
024300100729     C                   IF        PAR7U(YY) = ' '
024400100729     C                   LEAVE
024500100729     C                   END
024600100729     c                   eval      wpar = PAR7U(YY)
024700071228     c     kwfupa        chain     wfupa01l
024800071228      * - non è presente
024900071228     c                   if        not %found(wfupa01l)
025000071228     c                   clear                   wfupa000
025100071228      *   recupero i dati necessari da memorizzare sul file
025200071228     c                   exsr      sr_daticom
025300071228      *   particolarità giacenza
025400071228     c                   if        %subst(wtblkey:8:1) = 'Q'
025500071228     c                   exsr      sr_datigga
025600071228     c                   endif
025700071228      *   particolarità consegna
025800071228     c                   if        %subst(wtblkey:8:1) = 'R'
025900071228     c                   exsr      sr_datigma
026000071228     c                   endif
026100071228      *   particolarità varie
026200071228     c                   if        %subst(wtblkey:8:1) = 'S'
026300071228     c                   exsr      sr_datigva
026400071228     c                   endif
026500071228      *   scrivo nuovo record
026600071228     c                   exsr      sr_scrivinew
026700071228     c                   endif
026800100729     c                   endDO
026900100729     c                   endif
027000080103
027100080103      * mi riposiziono sulla tabella 7U
027200080103     c                   eval      kcod = '7U'
027300080103     c                   eval      kkey = wtblkey
027400080103     c     ktabel        setgt     tabel00f
027500071228
027600071228     c                   enddo
027700071228
027800071228     c                   endsr
027900071228
028000071228      //-----------------------------------------------------------------------
028100071228      //? Stampa fine
028200071228      //-----------------------------------------------------------------------
028300071228     c     sr_stpfine    begsr
028400071228
028500071228     c                   except    fine
028600071228
028700071228     c                   endsr
028800071228
028900071228      //-----------------------------------------------------------------------
029000071228      //? Controllo se record OK
029100071228      //-----------------------------------------------------------------------
029200071228     c     sr_recok      begsr
029300071228
029400071228     c                   eval      $recok = *off
029500071228
029600071228      * solo i tipi bolla validi
029700071228     c                   clear                   xx
029800071228     c                   eval      xx = %lookup(tastbl : stb)
029900071228     c                   if        xx > *zeros
030000071228      * per essere ok deve avere almeno una particolarità
030100071228     c                   if        tasgga <> *blanks or tasgma <> *blanks or
030200071228     c                             %subst(tasgva:1:1) <> *blanks
030300071228     c                   eval      $recok = *on
030400071228     c                   endif
030500071228     c                   endif
030600071228
030700071228     c                   endsr
030800071228
030900071228      //-----------------------------------------------------------------------
031000071228      //? Aggiorno il file di work
031100071228      //-----------------------------------------------------------------------
031200071228     c     sr_aggiorna   begsr
031300071228
031400071228     c                   add       1             upatsped
031500071228     c                   add       tasncl        upatcolli
031600071228     c                   add       tasvlf        upatvol
031700071228     c                   update    wfupa000
031800071228
031900071228     c                   endsr
032000071228
032100071228      //-----------------------------------------------------------------------
032200071228      //? Recupero i dati comuni da memorizzare sul file
032300071228      //-----------------------------------------------------------------------
032400071228     c     sr_daticom    begsr
032500071228
032600071228      *?recupero la ragione sociale del cliente
032700071228     c                   clear                   tibs69ds
032800071228     c                   clear                   ds_cnaco
032900071228     c                   clear                   ds_cnind
033000071228     c                   clear                   ds_cnclp
033100071228     c                   clear                   ds_fncls
033200071228     c                   eval      i69kac = wksc
033300121122     c                   eval      i69kcp = wksc
033400071228     c                   call      'TIBS69R'
033500071228     c                   parm                    tibs69ds
033600071228     c                   parm                    ds_cnaco
033700071228     c                   parm                    ds_cnind
033800071228     c                   parm                    ds_cnclp
033900071228     c                   parm                    ds_fncls
034000071228     c                   eval      upaksc  = wksc
034100071228     c                   eval      upakscd = acorag
034200121122     c                   eval      UPAdus  = CLPdus
034300071228
034400071228      *?recupero il cliente unificante
034500071228      * --> controllo se è padre
034600071228     c                   clear                   tibs10ds
034700071228     c                   eval      d10drf = *date
034800071228     c                   eval      d10tle = 'ST'
034900071228     c                   eval      d10paf = 'F'
035000071228     c                   eval      d10cod = upaksc
035100071228     c                   call      'TIBS10R'
035200071228     c                   parm                    tibs10ds
035300071228     c                   if        d10err = *blanks
035400080103     c                   z-add     d10cod        upakun
035500071228      * --> non è padre controllo se è figlio
035600071228     c                   else
035700071228     c                   eval      d10drf = *date
035800071228     c                   eval      d10tle = 'ST'
035900071228     c                   eval      d10paf = 'P'
036000071228     c                   eval      d10cod = upaksc
036100071228     c                   call      'TIBS10R'
036200071228     c                   parm                    tibs10ds
036300071228     c                   if        d10err = *blanks
036400071228     c                   z-add     d10cop        upakun
036500071228      * --> non è padre e nemmeno figlio
036600071228     c                   else
036700071228     c                   eval      upakun = upaksc
036800071228     c                   endif
036900071228     c                   endif
037000071228
037100071228      *?recupero la ragione sociale dell'unificante
037200071228     c                   if        upakun > *zeros
037300071228     c                   clear                   tibs69ds
037400071228     c                   clear                   ds_cnaco
037500071228     c                   clear                   ds_cnind
037600071228     c                   clear                   ds_cnclp
037700071228     c                   clear                   ds_fncls
037800071228     c                   eval      i69kac = upakun
037900071228     c                   call      'TIBS69R'
038000071228     c                   parm                    tibs69ds
038100071228     c                   parm                    ds_cnaco
038200071228     c                   parm                    ds_cnind
038300071228     c                   parm                    ds_cnclp
038400071228     c                   parm                    ds_fncls
038500071228     c                   eval      upakund = acorag
038600071228     c                   endif
038700071228
038800071228      *?recupero la filiale dell'unificante
038900071228     c                   movel     upakun        upafil
039000071228     c     upafil        chain     azorg01l
039100071228     c                   if        %found(azorg01l)
039200071228     c                   eval      upafild = orgdes
039300071228
039400071228      *?recupero l'area dell'unificante
039500071228     c                   eval      upacar = orgcar
039600071228     c                   eval      kcod = '05'
039700071228     c                   eval      kkey = %editc(upacar:'X')
039800071228     c     ktabel        chain     tabel00f
039900071228     c                   if        %found(tabel00f)
040000071228     c                   eval      upacard = tbluni
040100071228     c                   endif
040200071228
040300071228      *?recupero il distretto dell'unificante
040400071228     c                   eval      upadit = orgfl3
040500071228     c                   eval      kcod = '17'
040600071228     c                   eval      kkey = upadit
040700071228     c     ktabel        chain     tabel00f
040800071228     c                   if        %found(tabel00f)
040900071228     c                   eval      upaditd = tbluni
041000071228     c                   endif
041100071228
041200071228     c                   endif
041300100729      *?verifico se presente in tariffa
041400100729     c                   z-add     1             nn
041500100729     c     wpar          lookup    spar                                   88
041600100729     c                   if        *in88
041700100729     c                   eval      upatar = 'S'
041800100729     c                   else
041900100729     c                   eval      upatar = 'N'
042000100729     c                   end
042100100729     c                   endsr
042200100729
042300100729      //-----------------------------------------------------------------------
042400100729      // verifico se presente in tariffe
042500100729      //-----------------------------------------------------------------------
042600100729     c     sr_partar     begsr
042700100729     c                   clear                   spar
042800100729     c                   clear                   nn
042900100729     c     wksc          setll     tntam01l
043000100729     c                   do        *hival
043100100729     c     wksc          reade     tntam01l
043200100729     c                   if        %eof(tntam01l)
043300100729     c                   leave
043400100729     c                   end
043500100729     c* periodo
043600100729     c                   if        TAMddt > wdataal
043700100729     c                   iter
043800100729     c                   endif
043900100729     c                   if        TAMdst < wdatada
044000100729     c                   iter
044100100729     c* particolarità
044200100729     c                   endif
044300100729     c                   if        TAMGGA = ' ' and
044400100729     c                             TAMGMA =' ' and
044500100729     c                             TAMGVA = ' '
044600100729     c                   iter
044700100729     c                   endif
044800100729     c*
044900100729     c                   if        tamgga <> ' '
045000100729     c                   add       1             nn
045100100729     c                   eval      spar(nn)= tamgga
045200100729     c                   end
045300100729     c                   if        tamgma <> ' '
045400100729     c                   add       1             nn
045500100729     c                   eval      spar(nn)= tamgma
045600100729     c                   end
045700100729     c                   if        tamgva <> ' '
045800100729     c                   add       1             nn
045900100729     c                   eval      spar(nn)= tamgva
046000100729     c                   end
046100100729     c*
046200100729     c                   enddo
046300071228
046400071228     c                   endsr
046500071228
046600071228      //-----------------------------------------------------------------------
046700071228      //? Recupero i dati particolarità giacenza memorizzare sul file
046800071228      //-----------------------------------------------------------------------
046900071228     c     sr_datigga    begsr
047000071228
047100071228     c                   eval      upatppar = 'G'
047200071228     c                   eval      upacpar = wpar
047300071228      * cerco la descrizione della particolarità
047400071228     c                   clear                   ds7q
047500071228     c                   eval      kcod = '7Q'
047600071228     c                   eval      kkey = upacpar
047700071228     c     ktabel        chain     tabel00f
047800071228     c                   if        %found(tabel00f)
047900071228     c                   eval      ds7q = tbluni
048000100729     c                   eval      kcod = '7V'
048100071228     c                   eval      kkey = 'Q' + upacpar + 'V' + '0001'
048200071228     c                   clear                   ds7v
048300071228     c     ktabel        chain     tabel00f
048400071228     c                   if        %found(tabel00f)
048500071228     c                   eval      ds7v = tbluni
048600071228     c                   endif
048700071228     c                   eval      upacpard = §7vnot
048800071228      * determino se è da abilitare o meno
048900071228     c                   eval      upaflabi = §7qabc
049000071228     c                   endif
049100071228
049200071228     c                   endsr
049300071228
049400071228      //-----------------------------------------------------------------------
049500071228      //? Recupero i dati particolarità consegna memorizzare sul file
049600071228      //-----------------------------------------------------------------------
049700071228     c     sr_datigma    begsr
049800071228
049900071228     c                   eval      upatppar = 'C'
050000071228     c                   eval      upacpar = wpar
050100071228      * cerco la descrizione della particolarità
050200071228     c                   clear                   ds7r
050300071228     c                   eval      kcod = '7R'
050400071228     c                   eval      kkey = upacpar
050500071228     c     ktabel        chain     tabel00f
050600071228     c                   if        %found(tabel00f)
050700071228     c                   eval      ds7r = tbluni
050800071228     c                   eval      kcod = '7V'
050900071228     c                   eval      kkey = 'R' + upacpar + 'V' + '0001'
051000071228     c                   clear                   ds7v
051100071228     c     ktabel        chain     tabel00f
051200071228     c                   if        %found(tabel00f)
051300071228     c                   eval      ds7v = tbluni
051400071228     c                   endif
051500071228     c                   eval      upacpard = §7vnot
051600071228      * determino se è da abilitare o meno
051700071228     c                   eval      upaflabi = §7rabc
051800121122     c                   eval      UPAaut = §7Raut
051900071228     c                   endif
052000071228
052100071228     c                   endsr
052200071228
052300071228      //-----------------------------------------------------------------------
052400071228      //? Recupero i dati particolarità varie memorizzare sul file
052500071228      //-----------------------------------------------------------------------
052600071228     c     sr_datigva    begsr
052700071228
052800071228     c                   eval      upatppar = 'V'
052900071228     c                   eval      upacpar = wpar
053000071228      * cerco la descrizione della particolarità
053100071228     c                   clear                   ds7s
053200071228     c                   eval      kcod = '7S'
053300071228     c                   eval      kkey = upacpar
053400071228     c     ktabel        chain     tabel00f
053500071228     c                   if        %found(tabel00f)
053600071228     c                   eval      ds7s = tbluni
053700071228     c                   eval      kcod = '7V'
053800071228     c                   eval      kkey = 'S' + upacpar + 'V' + '0001'
053900071228     c                   clear                   ds7v
054000071228     c     ktabel        chain     tabel00f
054100071228     c                   if        %found(tabel00f)
054200071228     c                   eval      ds7v = tbluni
054300071228     c                   endif
054400071228     c                   eval      upacpard = §7vnot
054500071228      * determino se è da abilitare o meno
054600071228     c                   eval      upaflabi = §7sabc
054700071228     c                   endif
054800071228
054900071228     c                   endsr
055000071228
055100071228      //-----------------------------------------------------------------------
055200071228      //? Scrivo il file di work
055300071228      //-----------------------------------------------------------------------
055400071228     c     sr_scrivi     begsr
055500071228
055600071228     c                   eval      upapru = knmus
055700071228     c                   eval      upadtada = wdatada
055800071228     c                   eval      upadtaal = wdataal
055900071228     c                   add       1             upatsped
056000071228     c                   add       tasncl        upatcolli
056100071228     c                   add       tasvlf        upatvol
056200071228     c                   write     wfupa000
056300071228
056400071228     c                   endsr
056500071228
056600071228      //-----------------------------------------------------------------------
056700071228      //? Scrivo il file di work per i clienti senza bolle
056800071228      //-----------------------------------------------------------------------
056900071228     c     sr_scrivinew  begsr
057000071228
057100071228     c                   eval      upapru = knmus
057200071228     c                   eval      upadtada = wdatada
057300071228     c                   eval      upadtaal = wdataal
057400071228     c                   write     wfupa000
057500071228
057600071228     c                   endsr
057700071228
057800071228      //-----------------------------------------------------------------------
057900071228      //? Routine iniziale
058000071228      //-----------------------------------------------------------------------
058100071228     c     *inzsr        begsr
058200071228
058300071228     c     *entry        plist
058400071228     c                   parm                    kpjba
058500071228     c                   eval      param = kpjbu
058600071228
058700071228      * data del giorno
058800071228     c                   time                    wtime
058900071228     c                   move      wtime         wdate
059000071228
059100071228      * carico tabella 'TB' solo tipi 'N' o 'S'
059200071228     c                   clear                   xx
059300071228     c                   eval      kcod = 'TB'
059400071228     c     ktabel01      setll     tabel00f
059500071228     c                   do        *hival
059600071228     c     ktabel01      reade     tabel00f
059700071228     c                   if        %eof(tabel00f)
059800071228     c                   leave
059900071228     c                   endif
060000071228     c                   if        tblflg <> *blanks
060100071228     c                   iter
060200071228     c                   endif
060300071228     c                   eval      dstb = tbluni
060400071228     c                   if        §tbrbl <> 'N' and §tbrbl <> 'S'
060500071228     c                   iter
060600071228     c                   endif
060700071228     c                   add       1             xx
060800071228     c                   eval      stb(xx) = tblkey
060900071228     c                   eval      stp(xx) = §tbtpo
061000071228     c                   enddo
061100071228
061200071228     c     ktabel        klist
061300071228     c                   kfld                    codut
061400071228     c                   kfld                    kcod
061500071228     c                   kfld                    kkey
061600071228
061700071228     c     ktabel01      klist
061800071228     c                   kfld                    codut
061900071228     c                   kfld                    kcod
062000071228
062100071228     c     ktitas        klist
062200071228     c                   kfld                    waada
062300071228     c                   kfld                    wmgda
062400071228
062500071228     c     kwfupa        klist
062600071228     c                   kfld                    wksc
062700071228     c                   kfld                    wpar
062800071228
062900071228     c                   endsr
063000071228
063100071228      //-----------------------------------------------------------------------
063200071228     oqsysprt   e            testa          2 02
063300071228     o                                           20 'Statistica Utilizzo'
063400071228     o                                         +  1 'Particolarità'
063500071228     o                                         +  5 'Del:'
063600071228     o                       wdate         8   +  1
063700071228     o                                          120 'TNSS02R'
063800071228     o                       page          Z    132
063900071228     o                                          128 'Pag.'
064000071228     o          e            testa       3
064100071228     o                                           20 'Spedizioni dal   '
064200080103     o                       pdatada           +  1
064300071228     o                                         +  2 'al'
064400080103     o                       pdataal           +  1
064500071228     o                                         +  5 'Lanciata da '
064600071228     o                       knmus             +  1
064700071228     o          e            fine        3
064800071228     o                                            1 ' '
064900071228     o          e            fine        3
065000071228     o                                           30 '** Fine Elaborazione **'
065100071228      //-----------------------------------------------------------------------
065200071228
065300071228** cmd
065400080103CLRPFM FILE(WFUPA00F)
