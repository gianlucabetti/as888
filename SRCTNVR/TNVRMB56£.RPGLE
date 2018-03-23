000100060316     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200060315
000300060315      *------------------------------------------------------------------------*
000400060315      * VERIFICA/AGGIORNA CODICI IMPORTANZA CLIENTI
000500060315      *------------------------------------------------------------------------*
000600060315
000700060315     fazorg01l  if   e           k disk
000800060315     ftabel00f  if   e           k disk
000900060403     fwfcic03l  if   e           k disk    usropn rename(wfcic000:wfcic003)
001000060316     fwfcic05l  if   e           k disk    usropn rename(wfcic000:wfcic005)
001100060331     f                                     prefix(i_)
001200060324     ftist51p   o    e             printer oflind(*in90)
001300060316
001400060316      *------------------------------------------------------------------------*
001500060316      *  RIEPILOGO INDICATORI
001600060316      *------------------------------------------------------------------------*
001700060316      * 30 - comodo
001800060327      * 44 - categoria variata
001900060324      * 90 - salto pagina
002000060316      *------------------------------------------------------------------------*
002100060316
002200060316      *   V A R I A B I L I
002300060317     d codut           s              1  0 inz(1)
002400060316     d dataiso         s               d   datfmt(*iso)
002500060316     d depare          s                   like(wfcare)
002600060614     d depcmm          s                   like(wfccmm)
002700060316     d depdis          s                   like(wfcdis)
002800060316     d depfil          s                   like(wfcfil)
002900060316     d desare          s             20
003000060614     d descmm          s             25
003100060316     d desdis          s             20
003200060316     d desfil          s             20
003300060316     d finl00          s              1    inz(*off)
003400060316     d kcod            s                   like(tblcod)
003500060327     d kkcc            s                   like(clpkcc)
003600060316     d kkey            s                   like(tblkey)
003700060316     d kksc            s                   like(wfcksc)
003800060316     d lengh           s             15  5 inz(80)
003900060316     d sqlline         s           5000    varying
004000060316     d waamm           s                   like(i50ami)
004100060316     d wdata           s              8  0
004200060331     d wok             s              1    inz(*off)
004300060317     d wora            s                   like(wfcora)
004400060316     d wrkfat          s             12  3
004500060316     d w0030           s              3  0
004600060323     d w0040           s              4  0
004700060316     d w0070           s              7  0
004800060316     d xx              s              3  0
004900060317     d yy              s              3  0
005000060329     d zz              s              3  0
005100060316
005200060316      *   S C H I E R E
005300060316
005400060316      *   D S   I N T E R N E / E S T E R N E
005500060316
005600060316     d                 ds
005700060316     d  wmmi                   1      2  0
005800060316     d  waai                   3      6  0
005900060316     d  wmmaai                 1      6  0
006000060316
006100060316     d                 ds
006200060316     d  wmmf                   1      2  0
006300060316     d  waaf                   3      6  0
006400060316     d  wmmaaf                 1      6  0
006500060316
006600060316     d azuteds       e ds                  extname(azute00f)
006700060918     d cnacods       e ds                  extname(cnaco00f)
006800060918     d cnclpds       e ds                  extname(cnclp00f)
006900060918     d cnindds       e ds                  extname(cnind00f)
007000060316     d ddatiute      e ds
007100060614     d ds01          e ds
007200060918     d fnclsds       e ds                  extname(fncls00f)
007300060316     d kpjba         e ds
007400060323     d og143         e ds
007500060316     d tibs10ds      e ds
007600060316     d tibs34ds      e ds
007700060919     d tibs69ds      e ds
007800060918     d tibs73ds      e ds
007900060316     d tise70ds      e ds
008000060316     d tist50ds      e ds
008100060316
008200060316      *------------------------------------------------------------------------*
008300060316     c                   exsr      sr_stampa
008400060316
008500060316     c                   eval      *inlr = *on
008600060614
008700060316      *------------------------------------------------------------------------*
008800060316      * stampa
008900060316      *------------------------------------------------------------------------*
009000060316     c     sr_stampa     begsr
009100060331
009200060331      * apro i file di work
009300060403     c                   open      wfcic03l
009400060331     c                   open      wfcic05l
009500060316
009600060316     c                   eval      stpdai = wmmaai
009700060316     c                   eval      stpdaf = wmmaaf
009800060316     c                   eval      stpumes = i50umes
009900060331
010000060331     c                   if        i50agg = 'S'
010100060331     c                   eval      stpagg = 'SI'
010200060331     c                   else
010300060331     c                   eval      stpagg = 'NO'
010400060331     c                   endif
010500060331
010600060331     c                   if        i50var = 'S'
010700060331     c                   eval      stpvar = 'SI'
010800060331     c                   else
010900060331     c                   eval      stpvar = 'NO'
011000060331     c                   endif
011100060614
011200060614     c                   if        i50blc = 'S'
011300060614     c                   eval      stpblc = 'SI'
011400060614     c                   else
011500060614     c                   eval      stpblc = 'NO'
011600060614     c                   endif
011700060316
011800060316     c                   eval      stpitd = i50itd
011900060316     c                   eval      stpita = i50ita
012000060316     c                   eval      stpiad = i50iad
012100060316     c                   eval      stpiaa = i50iaa
012200060316     c                   eval      stpibd = i50ibd
012300060316     c                   eval      stpiba = i50iba
012400060316     c                   eval      stpicd = i50icd
012500060316     c                   eval      stpica = i50ica
012600060316     c                   eval      stpidd = i50idd
012700060316     c                   eval      stpida = i50ida
012800060331
012900060331      * stampo la pagina dei parametri
013000060331     c                   write     st51p01
013100060316
013200060616      *------
013300060616
013400060324      * leggo il file di work
013500060331     c     *loval        setll     wfcic05l
013600060324     c                   do        *hival
013700060331     c                   read      wfcic05l
013800060324      * fine file
013900060331     c                   if        %eof(wfcic05l)
014000060324     c                   leave
014100060324     c                   endif
014200060331
014300060331      * elaboro solo i codici unificanti e i non unificati
014400060331     c                   if        i_wfcflu = *blanks and i_wfcksu > 0
014500060331     c                   iter
014600060331     c                   endif
014700060331      * se non ha lavorato nei mesi richiesti
014800060331     c                   if        i_wfcchk <> *blanks
014900060331     c                   iter
015000060331     c                   endif
015100060331
015200060331      * per prima cosa devo verificare nel caso di unificante se variato lui o uno dei figli
015300060331      * perchè devo stampare sempre tutto il gruppo se anche solo 1 figlio è variato
015400060331     c                   if        i_wfcflu = 'X'
015500060331     c                   exsr      sr_ctrrcd
015600060331     c                   if        wok = *off
015700060331     c                   iter
015800060331     c                   endif
015900060331     c                   endif
016000090420
016100090420     c                   eval      *in45 = *off
016200060316
016300060331      * stampa riga di dettaglio se unificante
016400060331     c                   if        i_wfcflu  = 'X'
016500060331     c                   exsr      sr_stadetu
016600060331     c                   else
016700060331      * stampa riga di dettaglio se singolo
016800060331     c                   exsr      sr_stadet
016900060331     c                   endif
017000060404      * salto riga
017100090420     c   45              write     st51d02
017200060316
017300060324     c                   enddo
017400060316
017500060316      * stampo le righe di chiusura prospetto
017600060316     c                   write     st51ri
017700060316     c                   write     st51fi
017800060316
017900060316      * chiudo i file di work
018000060403     c                   close     wfcic03l
018100060331     c                   close     wfcic05l
018200060316
018300060316     c                   endsr
018400060331
018500060331      *------------------------------------------------------------------------*
018600060331      * controllo se il rcd è da stampare
018700060331      *------------------------------------------------------------------------*
018800060331     c     sr_ctrrcd     begsr
018900060331
019000060331     c                   eval      wok = *off
019100060331
019200060331      * se già l'unificante è diverso sono a posto
019300060331     c                   if        i_wfccca <> i_wfcccp
019400060331     c                   eval      wok = *on
019500060331     c                   leavesr
019600060331     c                   endif
019700060331
019800060403      * altrimenti controllo i figli
019900060403     c     i_wfcksu      setll     wfcic03l
020000060331     c                   do        *hival
020100060403     c     i_wfcksu      reade     wfcic03l
020200060403     c                   if        %eof(wfcic03l)
020300060331     c                   leave
020400060331     c                   endif
020500060331     c                   if        wfcccp <> wfccca
020600060331     c                   eval      wok = *on
020700060331     c                   leave
020800060331     c                   endif
020900060331     c                   enddo
021000060331
021100060331     c                   endsr
021200060616
021300060616      *------------------------------------------------------------------------*
021400060616      * stampa il dettaglio riga per unificante
021500060616      *------------------------------------------------------------------------*
021600060616     c     sr_stadetu    begsr
021700060616
021800060616      * imposto altri dati della stampa
021900060616     c                   clear                   st51d01
022000060616
022100060616     c                   eval      stpksu  = i_wfcksu
022200060616     c                   eval      stprau  = %trim(i_wfcrau)
022300060616     c                   eval      stpfatu = i_wfcfatu
022400060616     c                   eval      stpmesu = i_wfcmesiu
022500060616     c                   eval      stpfatp = i_wfcfatp
022600060616     c                   eval      stpblk  = i_wfcblk
022700060616     c                   eval      stpksc  = i_wfcksc
022800060616     c                   eval      stprag  = %trim(i_wfcrag)
022900060616     c                   eval      stpfat  = i_wfcfat
023000060616     c                   eval      stpmesi = i_wfcmesi
023100060616     c                   eval      stpcca  = i_wfccca
023200060616     c                   if        i_wfccca <> i_wfcccp
023300060616     c                   eval      stpccp  = i_wfcccp
023400060616     c                   endif
023500060616
023600060616      * stampo testata a rottura
023700060616     c                   exsr      sr_testa
023800060616      * se fine pagina salto pagina
023900060616     c                   if        *in90
024000060616     c                   write     st51t01
024100060616     c                   eval      *in90 = *off
024200060616     c                   endif
024300060616      * stampo il dettaglio dell'unificante
024400060616     c                   write     st51d01
024500090420     c                   eval      *in45 = *on
024600060616
024700060616      * reperisco i dati dei figli
024800060616     c                   clear                   st51d01
024900060616     c     i_wfcksu      setll     wfcic03l
025000060616     c                   do        *hival
025100060616     c     i_wfcksu      reade     wfcic03l
025200060616     c                   if        %eof(wfcic03l)
025300060616     c                   leave
025400060616     c                   endif
025500060616      * se stesso codice dell'unificante torno a leggere
025600060616     c                   if        wfcksu = wfcksc
025700060616     c                   iter
025800060616     c                   endif
025900060616     c                   eval      stpblk  = wfcblk
026000060616     c                   eval      stpksc  = wfcksc
026100060616     c                   eval      stprag  = %trim(wfcrag)
026200060616     c                   eval      stpfat  = wfcfat
026300060616     c                   eval      stpmesi = wfcmesi
026400060616     c                   eval      stpcca  = wfccca
026500060616     c                   clear                   stpccp
026600060616     c                   if        wfccca <> wfcccp
026700060616     c                   eval      stpccp  = wfcccp
026800060616     c                   endif
026900060616      * se richiesto non stampo se bloccato senza fatturato
027000060616     c                   if        i50blc = 'S' and stpblk = 'B' and stpfat = 0
027100060616     c                   iter
027200060616     c                   endif
027300060616      * se fine pagina salto pagina
027400060616     c                   if        *in90
027500060616     c                   write     st51t01
027600060616     c                   eval      *in90 = *off
027700060616     c                   endif
027800060616      * stampo il dettaglio dell'unificante
027900060616     c                   write     st51d01
028000060616     c                   enddo
028100060616
028200060616     c                   endsr
028300060616
028400060616      *------------------------------------------------------------------------*
028500060616      * stampa il dettaglio riga del singolo
028600060616      *------------------------------------------------------------------------*
028700060616     c     sr_stadet     begsr
028800060616
028900060616      * imposto altri dati della stampa
029000060616     c                   clear                   st51d01
029100060616     c                   eval      *in44 = *off
029200060616
029300060616     c                   eval      stpksu  = i_wfcksc
029400060616     c                   eval      stprau  = %trim(i_wfcrag)
029500060616     c                   eval      stpfatu = i_wfcfat
029600060616     c                   eval      stpmesu = i_wfcmesi
029700060616     c                   eval      stpfatp = i_wfcfatp
029800060616     c                   eval      stpblk  = i_wfcblk
029900060616     c                   eval      stpksc  = i_wfcksc
030000060616     c                   eval      stprag  = %trim(i_wfcrag)
030100060616     c                   eval      stpfat  = i_wfcfat
030200060616     c                   eval      stpmesi = i_wfcmesi
030300060616     c                   eval      stpcca  = i_wfccca
030400060616     c                   clear                   stpccp
030500060616     c                   if        i_wfccca <> i_wfcccp
030600060616     c                   eval      stpccp  = i_wfcccp
030700060616     c                   eval      *in44 = *on
030800060616     c                   endif
030900060616      * se richiesto non stampo se bloccato senza fatturato
031000060616     c                   if        i50blc = 'S' and stpblk = 'B' and stpfat = 0
031100060616     c                   leavesr
031200060616     c                   endif
031300060616
031400060616      * se richiesto stampo solo se categoria attuale <> dalla calcolata
031500060616     c                   if        i50var = 'N' or *in44
031600060616      * stampo testata a rottura
031700060616     c                   exsr      sr_testa
031800060616      * se fine pagina salto pagina
031900060616     c                   if        *in90
032000060616     c                   write     st51t01
032100060616     c                   eval      *in90 = *off
032200060616     c                   endif
032300090420      * stampo il dettaglio del cliente
032400090420     c                   write     st51d01
032500090420     c                   eval      *in45 = *on
032600060616     c                   endif
032700060616
032800060616     c                   endsr
032900060316
033000060316      *------------------------------------------------------------------------*
033100060616      * stampo testata
033200060316      *------------------------------------------------------------------------*
033300060616     c     sr_testa      begsr
033400060616
033500060616      * cambio distretto
033600060616     c                   if        i_wfcdis <> depdis
033700060616     c                   eval      depdis = i_wfcdis
033800060616     c                   exsr      sr_decdis
033900060616      * imposto il distretto in testata
034000060616     c                   eval      stpdis='DISTRETTO '+depdis+' '+desdis
034100060616     c                   eval      depare = i_wfcare
034200060616     c                   exsr      sr_decare
034300060616      * imposto l'area in testata
034400060616     c                   eval      stpare='AREA '+%editc(depare:'Z')+
034500060616     c                                    '  '+desare
034600060616     c                   eval      depfil = i_wfcfil
034700060616     c                   exsr      sr_decfil
034800060616      * imposto il p.o. in testata
034900060616     c                   eval      stpfil='P.O. '+%editc(depfil:'Z')+
035000060616     c                                    '  '+desfil
035100060616     c                   eval      depcmm = i_wfccmm
035200060616     c                   exsr      sr_deccmm
035300060616      * imposto il commerciale in testata
035400060627     c                   eval      stpcmm='COMMERCIALE UNIFICANTE '+
035500060627     c                                     %editc(depcmm:'Z')+
035600060627     c                                    '  '+descmm
035700060616      * stampo la testata
035800060616     c                   write     st51t01
035900060616     c                   endif
036000060616
036100060616      * cambio area
036200060616     c                   if        i_wfcare <> depare
036300060616     c                   eval      depdis = i_wfcdis
036400060616     c                   exsr      sr_decdis
036500060616      * imposto il distretto in testata
036600060616     c                   eval      stpdis='DISTRETTO '+depdis+' '+desdis
036700060616     c                   eval      depare = i_wfcare
036800060616     c                   exsr      sr_decare
036900060616      * imposto l'area in testata
037000060616     c                   eval      stpare='AREA '+%editc(depare:'Z')+
037100060616     c                                    '  '+desare
037200060616     c                   eval      depfil = i_wfcfil
037300060616     c                   exsr      sr_decfil
037400060616      * imposto il p.o. in testata
037500060616     c                   eval      stpfil='P.O. '+%editc(depfil:'Z')+
037600060616     c                                    '  '+desfil
037700060616     c                   eval      depcmm = i_wfccmm
037800060616     c                   exsr      sr_deccmm
037900060616      * imposto il commerciale in testata
038000060627     c                   eval      stpcmm='COMMERCIALE UNIFICANTE '+
038100060627     c                                     %editc(depcmm:'Z')+
038200060627     c                                    '  '+descmm
038300060616      * stampo la testata
038400060616     c                   write     st51t01
038500060616     c                   endif
038600060616
038700060616      * cambio p.o.
038800060616     c                   if        i_wfcfil <> depfil
038900060616     c                   eval      depdis = i_wfcdis
039000060616     c                   exsr      sr_decdis
039100060616      * imposto il distretto in testata
039200060616     c                   eval      stpdis='DISTRETTO '+depdis+' '+desdis
039300060616     c                   eval      depare = i_wfcare
039400060616     c                   exsr      sr_decare
039500060616      * imposto l'area in testata
039600060616     c                   eval      stpare='AREA '+%editc(depare:'Z')+
039700060616     c                                    '  '+desare
039800060616     c                   eval      depfil = i_wfcfil
039900060616     c                   exsr      sr_decfil
040000060616      * imposto il p.o. in testata
040100060616     c                   eval      stpfil='P.O. '+%editc(depfil:'Z')+
040200060616     c                                    '  '+desfil
040300060616     c                   eval      depcmm = i_wfccmm
040400060616     c                   exsr      sr_deccmm
040500060616      * imposto il commerciale in testata
040600060627     c                   eval      stpcmm='COMMERCIALE UNIFICANTE '+
040700060627     c                                     %editc(depcmm:'Z')+
040800060627     c                                    '  '+descmm
040900060616      * stampo la testata
041000060616     c                   write     st51t01
041100060616     c                   endif
041200060616
041300060616      * cambio commerciale
041400060616     c                   if        i_wfccmm <> depcmm
041500060616     c                   eval      depdis = i_wfcdis
041600060616     c                   exsr      sr_decdis
041700060616      * imposto il distretto in testata
041800060616     c                   eval      stpdis='DISTRETTO '+depdis+' '+desdis
041900060616     c                   eval      depare = i_wfcare
042000060616     c                   exsr      sr_decare
042100060616      * imposto l'area in testata
042200060616     c                   eval      stpare='AREA '+%editc(depare:'Z')+
042300060616     c                                    '  '+desare
042400060616     c                   eval      depfil = i_wfcfil
042500060616     c                   exsr      sr_decfil
042600060616      * imposto il p.o. in testata
042700060616     c                   eval      stpfil='P.O. '+%editc(depfil:'Z')+
042800060616     c                                    '  '+desfil
042900060616     c                   eval      depcmm = i_wfccmm
043000060616     c                   exsr      sr_deccmm
043100060616      * imposto il commerciale in testata
043200060627     c                   eval      stpcmm='COMMERCIALE UNIFICANTE '+
043300060627     c                                     %editc(depcmm:'Z')+
043400060627     c                                    '  '+descmm
043500060616      * stampo la testata
043600060616     c                   write     st51t01
043700060616     c                   endif
043800060616
043900060616     c                   endsr
044000060616      *------------------------------------------------------------------------*
044100060616      * decodifico il distretto
044200060616      *------------------------------------------------------------------------*
044300060616     c     sr_decdis     begsr
044400060316
044500060316      * decodifico il distretto
044600060324     c                   eval      kcod = '17'
044700060324     c                   eval      kkey = depdis
044800060324     c     ktabel        chain     tabel00f
044900060324     c                   if        not %found(tabel00f) or tblflg <> *blanks
045000060324     c                   eval      desdis = *all'*'
045100060324     c                   else
045200060324     c                   eval      desdis = tbluni
045300060324     c                   endif
045400060316
045500060316     c                   endsr
045600060324
045700060316      *------------------------------------------------------------------------*
045800060316      * decodifco area
045900060316      *------------------------------------------------------------------------*
046000060316     c     sr_decare     begsr
046100060316
046200060316     c                   eval      kcod = '05'
046300060317     c                   movel     depare        kkey
046400060316     c     ktabel        chain     tabel00f
046500060316     c                   if        not %found(tabel00f) or tblflg <> *blanks
046600060316     c                   eval      desare = *all'*'
046700060316     c                   else
046800060316     c                   eval      desare = tbluni
046900060316     c                   endif
047000060316
047100060316     c                   endsr
047200060316
047300060316      *------------------------------------------------------------------------*
047400060316      * decodifico p.o.
047500060316      *------------------------------------------------------------------------*
047600060316     c     sr_decfil     begsr
047700060316
047800060316     c     depfil        chain     azorg01l
047900060316     c                   if        not %found(azorg01l) or orgfva <> *blanks
048000060316     c                   eval      desfil = *all'*'
048100060316     c                   else
048200060316     c                   eval      desfil = orgdes
048300060316     c                   endif
048400060316
048500060316     c                   endsr
048600060614
048700060614      *------------------------------------------------------------------------*
048800060614      * decodifco commerciale
048900060614      *------------------------------------------------------------------------*
049000060614     c     sr_deccmm     begsr
049100060614
049200060614     c                   eval      kcod = '01'
049300060614     c                   movel     depcmm        kkey
049400060614     c     ktabel        chain     tabel00f
049500060614     c                   if        not %found(tabel00f) or tblflg <> *blanks
049600060614     c                   eval      descmm = *all'*'
049700060614     c                   else
049800060614     c                   eval      ds01 = tbluni
049900060614     c                   eval      descmm = §01age
050000060614     c                   endif
050100060614
050200060614     c                   endsr
050300060316
050400060316      *------------------------------------------------------------------------*
050500060316      * routine iniziale
050600060316      *------------------------------------------------------------------------*
050700060316     c     *inzsr        begsr
050800060316
050900060316     c     *entry        plist
051000060316     c                   parm                    kpjba
051100060316     c                   eval      tist50ds = kpjbu
051200060316
051300060316     c     *dtaara       define    §azute        azuteds
051400060316     c     *dtaara       define    §datiute      ddatiute
051500060316     c                   in(e)     *dtaara
051600060316     c                   if        %error  or rsut = *blanks
051700060316     c                   clear                   tibs34ds
051800060316     c                   call      'TIBS34R'
051900060316     c                   parm                    tibs34ds
052000060316     c                   in        *dtaara
052100060316     c                   endif
052200060316
052300060317     c                   movel(p)  rsut          dsfirs
052400060316
052500060316     c                   movel     i50ami        waai
052600060316     c                   move      i50ami        wmmi
052700060316     c                   movel     i50amf        waaf
052800060316     c                   move      i50amf        wmmf
052900060317
053000060317     c                   time                    wora
053100060316
053200060316     c     ktabel        klist
053300060316     c                   kfld                    codut
053400060316     c                   kfld                    kcod
053500060316     c                   kfld                    kkey
053600060327
053700060404     c     kcnclp        klist
053800060404     c                   kfld                    codut
053900060404     c                   kfld                    kkcc
054000060404     c                   kfld                    kksc
054100060316
054200060316     c                   endsr
