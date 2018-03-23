000100060320     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200060320      *------------------------------------------------------------------------*
000300060320      *
000400060320      *  Stato avanzamento progetto AC BASE
000500060320      *
000600060320      *------------------------------------------------------------------------*
000700060215
000800060215     ftntam04l  if   e           k disk
000900060215     ftitpt01l  if   e           k disk
001000060320     ftitpd01l  if   e           k disk
001100060320     fazorg01l  if   e           k disk
001200060320     fazorg02l  if   e           k disk    rename(azorg:azorg02)
001300060320     ftabel00f  if   e           k disk
001400060320     fwftac00f  o    e             disk    usropn prefix(f_)
001500060320     fwftac10f  o    e             disk    usropn rename(wftac000:wftac10)
001600060322     fwftac11l  if   e           k disk    usropn rename(wftac000:wftac11)
001700060322     fwftac12l  if   e           k disk    usropn rename(wftac000:wftac12)
001800060321     ftnta58p   o    e             printer oflind(*in90)
001900060323
002000060323      *------------------------------------------------------------------------*
002100060323      *  RIEPILOGO INDICATORI
002200060323      *------------------------------------------------------------------------*
002300060323      * 01 - stampa per ragione sociale
002400060323      * 02 - ho scritto almeno 1 rcd
002500060323      * 12 - utente di sede
002600060323      * 30 - comodo
002700060323      * 46 - stampo aree + relativi totali
002800060323      * 90 - salto pagina
002900060215
003000060215      *------------------------------------------------------------------------*
003100060215      *   V A R I A B I L I
003200060215      *------------------------------------------------------------------------*
003300060320     d comman          s             80
003400060320     d codut           s              1  0 inz(1)
003500060320     d kcar            s                   like(orgcar)
003600060320     d kcod            s                   like(tblcod)
003700060320     d kksc            s                   like(tamksc)
003800060320     d kkey            s                   like(tblkey)
003900060320     d lenght          s             15  5 inz(80)
004000060320     d kftc            s                   like(tptftc)
004100060215     d kkcc            s                   like(acokcc) inz(151)
004200060215     d kkut            s                   like(acokut) inz(1)
004300060321     d savksc          s                   like(tacksc)
004400060320     d wacbase         s              1    inz(*off)
004500060320     d wacplus         s              1    inz(*off)
004600060323     d wacplusc        s              1    inz(*off)
004700060320     d wacplusr        s              1    inz(*off)
004800060215     d wctr            s                   like(tamctr)
004900060320     d wdata           s              8  0
005000060320     d witrd           s                   like(tpditr)
005100060215     d wksc            s                   like(tamksc)
005200060320     d wlib            s              9
005300060324     d wok             s              1    inz(*off)
005400060320     d wora            s              6  0
005500060215     d wtptapl         s                   like(tptapl)
005600060215     d wtpttma         s                   like(tpttma)
005700060320     d w003a           s              3
005800060320     d w0030           s              3  0
005900060320     d w0140           s             14  0
006000060320     d xx              s              3  0
006100060320     d yy              s              3  0
006200060320
006300060320      *------------------------------------------------------------------------*
006400060320      *   S C H I E R E
006500060320      *------------------------------------------------------------------------*
006600060320     d cmd             s             80    dim(2) ctdata perrcd(1)
006700060320     d sknocl          s              3  0 dim(250)
006800060320     d skpoel          s              3    dim(250)
006900060215
007000060215      *------------------------------------------------------------------------*
007100060215      *   D S   I N T E R N E / E S T E R N E
007200060215      *------------------------------------------------------------------------*
007300060320     d dswftac0      e ds                  extname(wftac00f)
007400060320     d                                     prefix(f_)
007500060320     d                                     inz
007600060320     d dswftac1      e ds                  extname(wftac10f)
007700060320     d                                     inz
007800060320
007900060320     d parm01          ds
008000060321     d  parfil                 1      3  0
008100060321     d  parcar                 4     33    dim(10)
008200060321     d  parsc01               34     34
008300060321     d  parsc02               35     35
008400060321     d  parsc03               36     36
008500060321     d  parsc04               37     37
008600060323     d  parfile               38     38
008700060323     d  parord                39     39
008800060323     d  parabi                40     41
008900060321
009000060321     d wlbdat          ds                  inz
009100060321     d  g02dat                 1      8  0
009200060321     d  g02inv                 9     16  0
009300060321     d  g02err                17     17
009400060321     d  g02tgi                18     22  0
009500060320
009600060320     d azuteds       e ds                  extname(azute00f)
009700060320     d ddatiute      e ds
009800060726     d dfab          e ds
009900060320     d ds_cnaco      e ds                  inz  extname(Cnaco00f)
010000060320     d ds_cnind      e ds                  inz  extname(Cnind00f)
010100060320     d ds_cnclp      e ds                  inz  extname(Cnclp00f)
010200060320     d ds_fncls      e ds                  inz  extname(Fncls00f)
010300060215     d kpjba         e ds
010400060215     d dsta01        e ds
010500060320     d og143         e ds
010600060726     d tibs02ds      e ds
010700060320     d tibs34ds      e ds
010800060320     d tibs69ds      e ds
010900060320     d trul31ds      e ds                  inz
011000060215
011100060215      *------------------------------------------------------------------------*
011200060215
011300060320      * ciclo per i p.o. caricati in schiera
011400060320    1c                   do        250           xx
011500060320     c                   if        skpoel(xx) = *blanks
011600060320     c                   leave
011700060320     c                   endif
011800060320
011900060320     c                   clear                   wksc
012000060215     c                   eval      wctr = 999
012100060320     c                   move      *all'0'       kksc
012200060320     c                   movel     skpoel(xx)    kksc
012300060215
012400060320     c     kksc          setll     tntam04l
012500060320    2c                   do        *hival
012600060215     c                   read      tntam04l
012700060215      * fine file
012800060215     c                   if        %eof(tntam04l)
012900060215     c                   leave
013000060215     c                   endif
013100060320
013200060320      * cambio tariffa
013300060320    3c                   if        tamctr <> wctr or tamksc <> wksc
013400060320
013500060320     c                   movel     tamflo        dsta01
013600060320
013700060320      * se p.o. maggiore esco
013800060320     c                   movel     tamksc        w003a
013900060320     c                   if        w003a > skpoel(xx)
014000060320     c                   leave
014100060320     c                   endif
014200060320      * escludo le annullate
014300060215     c                   if        tamatb <> *blanks
014400060215     c                   iter
014500060215     c                   endif
014600060320      * escludo le bloccate
014700060320     c                   if        tambap = 'B'
014800060320     c                   iter
014900060320     c                   endif
015000060320      * escludo le tariffe EEX - DPD - FED
015100060320     c                   if        tamfie = 'E' or §tadpd = 'S' or
015200060320     c                             §tafed = 'S'
015300060320     c                   iter
015400060320     c                   endif
015500060320
015600060320      * cerco i dati anagrafici
015700060320     c                   clear                   tibs69ds
015800060320     c                   move      tamksc        i69kac
015900060320     c                   move      tamksc        i69kin
016000060320     c                   move      tamksc        i69kcp
016100060320     c                   call      'TIBS69R'
016200060320     c                   parm                    tibs69ds
016300060320     c                   parm                    ds_cnaco
016400060320     c                   parm                    ds_cnind
016500060320     c                   parm                    ds_cnclp
016600060320     c                   parm                    ds_fncls
016700060320      * escludo le tariffe dei clienti bloccati
016800060320     c                   if        acoabl = '8' or acoabl = '*'
016900060320     c                   iter
017000060320     c                   endif
017100060323      * escludo le tariffe dei clienti PPT - DPD - EEX - FED
017200060320     c                   movel     tamksc        w0030
017300060320     c     w0030         lookup    sknocl                                 30
017400060320     c                   if        *in30
017500060320     c                   iter
017600060320     c                   endif
017700060320      * escludo le tariffe dei clienti che non hanno avuto spedizioni fatturate
017800060320      * dopo il 1/1/2005
017900060321     c                   if        clpdus > *zeros
018000060321      * controllo la data lunga 8
018100060321     c                   clear                   wlbdat
018200060321     c                   z-add     clpdus        g02inv
018300060321     c                   eval      g02err = '3'
018400060321     c                   call      'XSRDA8'
018500060321     c                   parm                    wlbdat
018600060321     c                   if        g02inv < 20050101
018700060321     c                   iter
018800060321     c                   endif
018900060321     c                   endif
019000060320
019100060320      * cerco i dati delle tariffe particolari
019200060320     c                   eval      wacbase = *off
019300060320     c                   clear                   witrd
019400060320     c                   eval      wacplus = *off
019500060323     c                   eval      wacplusc = *off
019600060320     c                   clear                   wtpttma
019700060320     c                   clear                   wtptapl
019800060320     c                   eval      wacplusr = *off
019900060320
020000060320      * controllo se AC BASE presente
020100060320     c                   eval      kftc = 'd '
020200060320     c     ktpt          setll     titpt01l
020300060320    4c                   do        *hival
020400060320     c     ktpt          reade     titpt01l
020500060320      * fine file
020600060320     c                   if        %eof(titpt01l)
020700060320     c                   leave
020800060320     c                   endif
020900060320      * non annullate
021000060320     c                   if        tptatb <> *blanks
021100060320     c                   iter
021200060320     c                   endif
021300060320     c                   eval      wacbase = *on
021400060320      * cerco importo AC BASE
021500060320     c     ktpt          setll     titpd01l
021600060320    5c                   do        *hival
021700060320     c     ktpt          reade     titpd01l
021800060320      * fine file
021900060320     c                   if        %eof(titpd01l)
022000060320     c                   leave
022100060320     c                   endif
022200060320      * non annullate
022300060320     c                   if        tpdatb <> *blanks
022400060320     c                   iter
022500060320     c                   endif
022600060320     c                   eval      witrd = tpditr
022700060320     c                   leave
022800060320    5c                   enddo
022900060320    4c                   enddo
023000060320
023100060320      * controllo se AC PLUS presente
023200060321     c                   eval      kftc = 'C '
023300060320     c     ktpt          setll     titpt01l
023400060320    4c                   do        *hival
023500060320     c     ktpt          reade     titpt01l
023600060320     c                   if        %eof(titpt01l)
023700060320     c                   leave
023800060320     c                   endif
023900060320      * non annullate
024000060320     c                   if        tptatb <> *blanks
024100060320     c                   iter
024200060320     c                   endif
024300060323     c                   eval      wacplus = *on
024400060320      * AC PLUS reale/fittizia
024500060320     c                   eval      wtpttma = tpttma
024600060320      * AC PLUS par/arr/entrambe
024700060320     c                   eval      wtptapl = tptapl
024800060320      * controllo se AC PLUS reale e completa
024900060320     c                   if        tpttma = *blanks
025000060320     c                   select
025100060321     c                   when      tptapl = *blanks
025200060323     c                   eval      wacplusc = *on
025300060322     c                   when      tambap = 'P' and tptapl = 'P'
025400060323     c                   eval      wacplusc = *on
025500060322     c                   when      tambap = 'A' and tptapl = 'A'
025600060323     c                   eval      wacplusc = *on
025700060320     c                   endsl
025800060320     c                   leave
025900060321     c                   endif
026000060320    4c                   enddo
026100060320
026200060324      * controllo se AC PLUS 8 presente
026300060320     c                   eval      kftc = '8 '
026400060320     c     ktpt          setll     titpt01l
026500060320    4c                   do        *hival
026600060320     c     ktpt          reade     titpt01l
026700060320     c                   if        %eof(titpt01l)
026800060320     c                   leave
026900060320     c                   endif
027000060320      * non annullate
027100060320     c                   if        tptatb <> *blanks
027200060320     c                   iter
027300060320     c                   endif
027400060320     c                   eval      wacplusr = *on
027500060320     c                   leave
027600060320    4c                   enddo
027700060320
027800060324      * scrivo il file solo se:
027900060320     c                   select
028000060320      * sottoscrizione AC BASE
028100060323     c                   when      wacbase = *on
028200060320     c                   exsr      scrivi
028300060320      * rinuncia AC BASE
028400060323     c                   when      tamfmp = 'S'
028500060320     c                   exsr      scrivi
028600060320      * nessuna risposta
028700060323     c                   when      tamfmp = *blanks and
028800060323     c                             wacbase = *off and wacplusc = *off
028900060320     c                   exsr      scrivi
029000060323      * AC PLUS inserita
029100060323     c                   when      wacplus = *on
029200060323     c                   exsr      scrivi
029300060323     c                   endsl
029400060215
029500060320     c                   eval      wksc = tamksc
029600060320     c                   eval      wctr = tamctr
029700060215
029800060215      * se la tariffa che sto leggendo deve ancora andare in vigore la elaboro
029900060215      * poi però devo elaborare anche la tariffa precedente che è in vigore ora
030000060215     c                   if        tamddt > *date
030100060215     c                   eval      wctr = 999
030200060215     c                   endif
030300060320    3c                   endif
030400060320
030500060320    2c                   enddo
030600060215
030700060320    1c                   enddo
030800060320
030900060320      * stampo
031000060323     c                   if        parfile <> 'F'
031100060323     c   02              exsr      sr_stampa
031200060323     c  n02              exsr      sr_selez
031300060323     c  n02              write     ta58e1
031400060321     c                   endif
031500060215
031600060215     c                   eval      *inlr = *on
031700060215
031800060215      *------------------------------------------------------------------------*
031900060320      * SCRIVO IL FILE PER LA STAMPA
032000060215      *------------------------------------------------------------------------*
032100060215     c     scrivi        begsr
032200060215
032300060323     c  n02              eval      *in02 = *on
032400060323
032500060320     c                   clear                   wftac10
032600060320
032700060320     c                   movel     tamksc        w0030
032800060320     c     w0030         chain     azorg01l
032900060215
033000060320      * utente/data/ora elaborazione
033100060320     c                   eval      tacpru = knmus
033200060321     c                   eval      tacdte = wdata
033300060321     c                   eval      tacore = wora
033400060320      * distretto
033500060320     c                   eval      tacdis = orgfl3
033600060320     c                   eval      kcod = '17'
033700060320     c                   eval      kkey = tacdis
033800060320     c     ktabel        chain     tabel00f
033900060320     c                   if        not %found(tabel00f) or tblflg <> *blanks
034000060320     c                   eval      tacddis = *all'*'
034100060320     c                   else
034200060320     c                   eval      tacddis = tbluni
034300060320     c                   endif
034400060320      * area
034500060321     c                   eval      taccar = orgcar
034600060320     c                   eval      kcod = '05'
034700060321     c                   movel     taccar        kkey
034800060320     c     ktabel        chain     tabel00f
034900060320     c                   if        not %found(tabel00f) or tblflg <> *blanks
035000060321     c                   eval      tacdcar = *all'*'
035100060320     c                   else
035200060321     c                   eval      tacdcar = tbluni
035300060320     c                   endif
035400060320      * p.o.
035500060320     c                   eval      tacfil = orgfil
035600060320     c                   if        not %found(azorg01l) or orgfva <> *blanks
035700060321     c                   eval      tacdfil = *all'*'
035800060320     c                   else
035900060320     c                   eval      tacdfil = orgdes
036000060320     c                   endif
036100060320      * commerciale
036200060320     c                   eval      taccmm = clpage
036300060320     c                   eval      kcod = '01'
036400060320     c                   movel     taccmm        kkey
036500060320     c     ktabel        chain     tabel00f
036600060320     c                   if        not %found(tabel00f) or tblflg <> *blanks
036700060320     c                   eval      tacdcmm = *all'*'
036800060320     c                   else
036900060320     c                   eval      tacdcmm = tbluni
037000060320     c                   endif
037100060215      * cliente
037200060320     c                   eval      tacksc = tamksc
037300060320     c                   eval      tacrag = acorag
037400060320      * indirizzo
037500060320     c                   eval      tacvia = indvia
037600060320     c                   eval      taccit = indcit
037700060322     c                   eval      taccap = indcae
037800060320     c                   eval      tacprv = indprv
037900060320     c                   eval      tacnaz = indsta
038000060321      * telefono
038100060321     c                   eval      tactel = indtel
038200060215      * partita IVA/codice fiscale
038300060215     c                   if        indiva <> *Blanks
038400060320     c                   eval      tacivacf = indiva
038500060215     c                   else
038600060215     c                   if        indcdf <> *Blanks
038700060320     c                   eval      tacivacf = indcdf
038800060215     c                   endif
038900060215     c                   endif
039000060320      * importanza cliente
039100060320     c                   eval      tacclv = clpclv
039200060215      * data ultima spedizione fattura
039300060320     c                   eval      tacdus = clpdus
039400060215      * tariffa
039500060320     c                   eval      tacctr = tamctr
039600060320     c                   eval      tacprg = tamprg
039700060320     c                   eval      tacddt = tamddt
039800060320     c                   eval      tacdst = tamdst
039900060323      * tariffa bloccata partenza/arrivo
040000060322     c                   if        tambap = *blanks
040100060322     c                   eval      tacbap = 'E'
040200060215     c                   else
040300060322     c                   eval      tacbap = tambap
040400060215     c                   endif
040500060320      * rifiuta AC BASE
040600060320     c                   if        tamfmp = 'S'
040700060320     c                   eval      tacfmp = '1'
040800060320     c                   else
040900060320     c                   eval      tacfmp = '0'
041000060320     c                   endif
041100060320      * presente AC BASE
041200060320     c                   if        wacbase = *on
041300060320     c                   eval      tacacb = '1'
041400060320     c                   else
041500060320     c                   eval      tacacb = '0'
041600060320     c                   endif
041700060323      * presente AC PLUS
041800060323     c                   if        wacplus = *on
041900060323      * completa
042000060323     c                   if        wacplusc = *on
042100060320     c                   eval      tacacp = '1'
042200060323     c                   else
042300060323     c                   eval      tacacp = '0'
042400060323     c                   endif
042500060321      * AC PLUS reale/fittizia
042600060321     c                   if        wtpttma = *blanks
042700060321     c                   eval      tactmaacp = 'R'
042800060321     c                   else
042900060321     c                   eval      tactmaacp = 'F'
043000060321     c                   endif
043100060321      * AC PLUS par/arr/entr.
043200060321     c                   if        wtptapl = *blanks
043300060321     c                   eval      tacaplacp = 'E'
043400060321     c                   else
043500060321     c                   eval      tacaplacp = wtptapl
043600060321     c                   endif
043700060324     c                   else
043800060324     c                   eval      tacacp = '0'
043900060320     c                   endif
044000060320      * nessuna risposta
044100060323     c                   if        wacbase = *off and wacplusc = *off and
044200060320     c                             tamfmp = *blanks
044300060323     c                   eval      tacnorisp = '1'
044400060320     c                   else
044500060323     c                   eval      tacnorisp = '0'
044600060320     c                   endif
044700060323      * presente AC PLUS 8
044800060320     c                   if        wacplusr = *on
044900060320     c                   eval      tacac8 = '1'
045000060320     c                   else
045100060320     c                   eval      tacac8 = '0'
045200060320     c                   endif
045300060726
045400060320      * discordanza AC BASE con CLV
045500060321     c                   if        wacbase = *on
045600060726      * per prima cosa controllo se il cliente è presente nella tabella della
045700060726      * forzatura AC BASE in modo da fare il confronto con quanto inserito
045800060726      * in tabella
045900060726     c                   clear                   tibs02ds
046000060726     c                   clear                   dfab
046100060726     c                   eval      t02mod = 'C'
046200060726     c                   eval      t02sif = knsif
046300060726     c                   eval      t02cod = 'FAB'
046400060726     c                   movel     tamksc        t02ke1
046500060726     c                   call      'TIBS02R'
046600060726     c                   parm                    kpjba
046700060726     c                   parm                    tibs02ds
046800060726      * se lo trovo faccio il confronto e non faccio il confronto con CLV
046900060726     c                   if        t02err = *blanks
047000060726     c                   eval      dfab = t02uni
047100060726     c                   if        d§fabitr > 0 and witrd < d§fabitr
047200060726     c                   eval      tacdisc = '1'
047300060726     c                   else
047400060726     c                   eval      tacdisc = '0'
047500060726     c                   endif
047600060726     c                   goto      noclv
047700060726     c                   endif
047800060726      * controllo con CLV
047900060320     c                   select
048000060320      * clienti Top o Direzionali
048100060320     c                   when      (clpclv = 'T' or clpclv = 'D') and
048200060323     c                             witrd < 0,002
048300060320     c                   eval      tacdisc = '1'
048400060320      * clienti A
048500060323     c                   when      clpclv = 'A' and witrd < 0,003
048600060320     c                   eval      tacdisc = '1'
048700060320      * clienti C o B
048800060320     c                   when      (clpclv = 'C' or clpclv = 'B') and
048900060323     c                             witrd < 0,005
049000060320     c                   eval      tacdisc = '1'
049100060320     c                   other
049200060320     c                   eval      tacdisc = '0'
049300060320     c                   endsl
049400060321     c                   endif
049500060726     c     noclv         tag
049600060726
049700060320      * importo AC BASE
049800060320     c                   eval      tacitracb = witrd
049900060215      * scrivo file
050000060320     c                   write     wftac10
050100060320
050200060320      * se richiesto genero il file di work
050300060321     c                   if        parfile <> 'S'
050400060320     c                   clear                   wftac000
050500060320     c                   eval      dswftac0 = dswftac1
050600060320     c                   write     wftac000
050700060320     c                   endif
050800060215
050900060215     c                   endsr
051000060320
051100060320      *--------------------------------------------------------------*
051200060320      * STAMPO
051300060320      *--------------------------------------------------------------*
051400060320     c     sr_stampa     begsr
051500060321
051600060321      * stampo la pagina con le selezioni
051700060321     c                   exsr      sr_selez
051800060320
051900060320      * leggo file di comodo per la stampa
052000060323     c  n01*loval        setll     wftac11l
052100060323     c   01*loval        setll     wftac12l
052200060321     c                   do        *hival
052300060323     c  n01              read      wftac11l
052400060323     c   01              read      wftac12l
052500060322     c                   if        %eof
052600060320     c                   leave
052700060321     c                   endif
052800060324      * controllo se è da stampare in base alle selezioni fatte
052900060324     c                   exsr      sr_ctrrec
053000060324     c                   if        wok = *on
053100060324     c                   iter
053200060324     c                   endif
053300060321
053400060320      * rottura per distretto
053500060321     c                   if        pdis <> tacdis
053600060321      * totali per distretto
053700060322     c                   if        pdis <> *blanks and *in12
053800060324      * prima stampo il totale del p.o.
053900060322     c                   write     ta58tf1
054000060322     c                   clear                   ptfksc
054100060322     c                   clear                   ptfctr
054200060322     c                   clear                   ptffmp
054300060322     c                   clear                   ptfacb
054400060322     c                   clear                   ptfacp
054500060322     c                   clear                   ptfrisp
054600060322     c                   clear                   ptfdisc
054700060324      * salto pagina
054800060324     c                   write     ta58t1
054900060324     c                   write     ta58t2
055000060324     c                   write     ta58t3
055100060324      * poi stampo il totale area
055200060322     c                   write     ta58tc1
055300060322     c                   clear                   ptcksc
055400060322     c                   clear                   ptcctr
055500060322     c                   clear                   ptcfmp
055600060322     c                   clear                   ptcacb
055700060322     c                   clear                   ptcacp
055800060322     c                   clear                   ptcrisp
055900060322     c                   clear                   ptcdisc
056000060324      * salto pagina
056100060324     c                   write     ta58t1
056200060324     c                   write     ta58t2
056300060324     c                   write     ta58t3
056400060324      * infine stampo il totale del distretto
056500060321     c                   write     ta58td1
056600060321     c                   clear                   ptdksc
056700060321     c                   clear                   ptdctr
056800060321     c                   clear                   ptdfmp
056900060321     c                   clear                   ptdacb
057000060321     c                   clear                   ptdacp
057100060321     c                   clear                   ptdrisp
057200060321     c                   clear                   ptddisc
057300060321     c                   endif
057400060324      * stampo la testata
057500060321     c                   eval      pdis = tacdis
057600060321     c                   eval      pdesdis = tacddis
057700060321     c                   eval      pcar = taccar
057800060321     c                   eval      pdescar = tacdcar
057900060321     c                   eval      pfil = tacfil
058000060321     c                   eval      pdesfil = tacdfil
058100060321     c                   eval      pcmm = taccmm
058200060321     c                   eval      pdescmm = tacdcmm
058300060321     c                   write     ta58t1
058400060321     c                   write     ta58t2
058500060321     c                   write     ta58t3
058600060321     c                   write     ta58t4
058700060321     c                   endif
058800060320      * rottura per area
058900060322     c                   if        pcar <> taccar
059000060321      * totali per area
059100060323     c                   if        pcar <> *zeros and (*in46 or *in12)
059200060324      * prima stampo il totale del p.o.
059300060322     c                   write     ta58tf1
059400060322     c                   clear                   ptfksc
059500060322     c                   clear                   ptfctr
059600060322     c                   clear                   ptffmp
059700060322     c                   clear                   ptfacb
059800060322     c                   clear                   ptfacp
059900060322     c                   clear                   ptfrisp
060000060322     c                   clear                   ptfdisc
060100060324      * salto pagina
060200060324     c                   write     ta58t1
060300060324     c                   write     ta58t2
060400060324     c                   write     ta58t3
060500060324      * poi stampo il totale area
060600060321     c                   write     ta58tc1
060700060321     c                   clear                   ptcksc
060800060321     c                   clear                   ptcctr
060900060321     c                   clear                   ptcfmp
061000060321     c                   clear                   ptcacb
061100060321     c                   clear                   ptcacp
061200060321     c                   clear                   ptcrisp
061300060321     c                   clear                   ptcdisc
061400060321     c                   endif
061500060324      * stampo la testata
061600060321     c                   eval      pdis = tacdis
061700060321     c                   eval      pdesdis = tacddis
061800060321     c                   eval      pcar = taccar
061900060321     c                   eval      pdescar = tacdcar
062000060321     c                   eval      pfil = tacfil
062100060321     c                   eval      pdesfil = tacdfil
062200060321     c                   eval      pcmm = taccmm
062300060321     c                   eval      pdescmm = tacdcmm
062400060321     c                   write     ta58t1
062500060321     c                   write     ta58t2
062600060321     c                   write     ta58t3
062700060321     c                   write     ta58t4
062800060321     c                   endif
062900060320      * rottura per p.o.
063000060321     c                   if        pfil <> tacfil
063100060324      * totali per p.o.
063200060321     c                   if        pfil <> *zeros
063300060321     c                   write     ta58tf1
063400060321     c                   clear                   ptfksc
063500060321     c                   clear                   ptfctr
063600060321     c                   clear                   ptffmp
063700060321     c                   clear                   ptfacb
063800060321     c                   clear                   ptfacp
063900060321     c                   clear                   ptfrisp
064000060321     c                   clear                   ptfdisc
064100060321     c                   endif
064200060321      * testata
064300060321     c                   eval      pdis = tacdis
064400060321     c                   eval      pdesdis = tacddis
064500060321     c                   eval      pcar = taccar
064600060321     c                   eval      pdescar = tacdcar
064700060321     c                   eval      pfil = tacfil
064800060321     c                   eval      pdesfil = tacdfil
064900060321     c                   eval      pcmm = taccmm
065000060321     c                   eval      pdescmm = tacdcmm
065100060321     c                   write     ta58t1
065200060321     c                   write     ta58t2
065300060321     c                   write     ta58t3
065400060321     c                   write     ta58t4
065500060320     c                   endif
065600060321
065700060321      * rottura per commerciale
065800060321     c                   if        pcmm <> taccmm
065900060321     c                   eval      pdis = tacdis
066000060321     c                   eval      pdesdis = tacddis
066100060321     c                   eval      pcar = taccar
066200060321     c                   eval      pdescar = tacdcar
066300060321     c                   eval      pfil = tacfil
066400060321     c                   eval      pdesfil = tacdfil
066500060321     c                   eval      pcmm = taccmm
066600060321     c                   eval      pdescmm = tacdcmm
066700060321      * salto pagina
066800060321     c                   if        *in90 = *on
066900060321     c                   write     ta58t1
067000060321     c                   write     ta58t2
067100060321     c                   write     ta58t3
067200060321     c                   write     ta58t4
067300060321     c                   eval      *in90 = *off
067400060321     c                   else
067500060322     c                   write     ta58t4
067600060321     c                   endif
067700060321     c                   endif
067800060321
067900060321      * conto quanti clienti
068000060321     c                   if        savksc <> tacksc
068100060321     c                   add       1             tksc
068200060321     c                   add       1             ptdksc
068300060321     c                   add       1             ptcksc
068400060321     c                   add       1             ptfksc
068500060321     c                   eval      savksc = tacksc
068600060321     c                   endif
068700060321      * conto quante tariffe
068800060321     c                   add       1             tctr
068900060321     c                   add       1             ptdctr
069000060321     c                   add       1             ptcctr
069100060321     c                   add       1             ptfctr
069200060321
069300060321      * ragione sociale
069400060321     c                   eval      prag = tacrag
069500060321      * validità tariffa
069600060322     c                   clear                   pbap
069700060321     c                   select
069800060322     c                   when      tacbap = 'P'
069900060322     c                   eval      pbap = 'PAR'
070000060322     c                   when      tacbap = 'A'
070100060322     c                   eval      pbap = 'ARR'
070200060322     c                   when      tacbap = 'E'
070300060323     c                   eval      pbap = 'P+A'
070400060321     c                   endsl
070500060321      * rifiuta AC BASE
070600060321     c                   clear                   pfmp
070700060321     c                   if        tacfmp = '0'
070800060321     c                   eval      pfmp = 'NO'
070900060321     c                   endif
071000060321     c                   if        tacfmp = '1'
071100060321     c                   eval      pfmp = 'SI'
071200060321     c                   add       1             tfmp
071300060321     c                   add       1             ptdfmp
071400060321     c                   add       1             ptcfmp
071500060321     c                   add       1             ptffmp
071600060321     c                   endif
071700060321      * presente AC BASE
071800060321     c                   clear                   pacb
071900060321     c                   if        tacacb = '0'
072000060321     c                   eval      pacb = 'NO'
072100060321     c                   endif
072200060321     c                   if        tacacb = '1'
072300060321     c                   eval      pacb = 'SI'
072400060321     c                   add       1             tacb
072500060321     c                   add       1             ptdacb
072600060321     c                   add       1             ptcacb
072700060321     c                   add       1             ptfacb
072800060321     c                   endif
072900060324      * presente AC PLUS completa
073000060321     c                   clear                   pacp
073100060321     c                   if        tacacp = '0'
073200060321     c                   eval      pacp = 'NO'
073300060321     c                   endif
073400060321     c                   if        tacacp = '1'
073500060321     c                   eval      pacp = 'SI'
073600060321     c                   add       1             tacp
073700060321     c                   add       1             ptdacp
073800060321     c                   add       1             ptcacp
073900060321     c                   add       1             ptfacp
074000060321     c                   endif
074100060321      * validità AC PLUS
074200060321     c                   clear                   papl
074300060324     c                   if        tactmaacp = 'R'
074400060321     c                   select
074500060321     c                   when      tacaplacp = 'P'
074600060321     c                   eval      papl = 'PAR'
074700060321     c                   when      tacaplacp = 'A'
074800060321     c                   eval      papl = 'ARR'
074900060321     c                   when      tacaplacp = 'E'
075000060323     c                   eval      papl = 'P+A'
075100060321     c                   endsl
075200060324     c                   endif
075300060321      * risposta
075400060321     c                   clear                   prisp
075500060323     c                   if        tacnorisp = '1'
075600060321     c                   eval      prisp = 'NO'
075700060321     c                   add       1             trisp
075800060321     c                   add       1             ptdrisp
075900060321     c                   add       1             ptcrisp
076000060321     c                   add       1             ptfrisp
076100060321     c                   endif
076200060323     c                   if        tacnorisp = '0'
076300060321     c                   eval      prisp = 'SI'
076400060321     c                   endif
076500060321      * discordanza
076600060321     c                   clear                   pdisc
076700060321     c                   if        tacdisc = '0'
076800060321     c                   eval      pdisc = 'NO'
076900060321     c                   endif
077000060321     c                   if        tacdisc = '1'
077100060321     c                   eval      pdisc = 'SI'
077200060321     c                   add       1             tdisc
077300060321     c                   add       1             ptddisc
077400060321     c                   add       1             ptcdisc
077500060321     c                   add       1             ptfdisc
077600060321     c                   endif
077700060321      * importo AC BASE
077800060321     c                   eval      pitr = tacitracb
077900060321      * salto pagina
078000060324     c                   if        *in90 = *on
078100060321     c                   write     ta58t1
078200060321     c                   write     ta58t2
078300060321     c                   write     ta58t3
078400060321     c                   write     ta58t4
078500060321     c                   eval      *in90 = *off
078600060321     c                   endif
078700060321      * stampo dettaglio
078800060321     c                   write     ta58d1
078900060320
079000060320 e1  c                   enddo
079100060321
079200060321      * totale generale
079300060324      * prima stampo il totale del p.o.
079400060322     c                   write     ta58tf1
079500060324      * salto pagina
079600060324     c                   if        *in12 or *in46
079700060324     c                   write     ta58t1
079800060324     c                   write     ta58t2
079900060324     c                   write     ta58t3
080000060324      * stampo il totale dell'area
080100060324     c                   write     ta58tc1
080200060324     c                   endif
080300060324      * salto pagina
080400060324     c                   if        *in12
080500060324     c                   write     ta58t1
080600060324     c                   write     ta58t2
080700060324     c                   write     ta58t3
080800060324      * stampo il totale del distretto
080900060324     c                   write     ta58td1
081000060324      * salto pagina
081100060324     c                   write     ta58t1
081200060324     c                   write     ta58t2
081300060324     c                   write     ta58t3
081400060324      * stampo il totale generale
081500060324     c                   write     ta58tt
081600060324     c                   endif
081700060320
081800060320      * fine stampa
081900060321     c                   write     ta58e1
082000060320
082100060320     c                   endsr
082200060321
082300060321      *--------------------------------------------------------------*
082400060321      * STAMPO LA PAGINA DELLE SELEZIONI
082500060321      *--------------------------------------------------------------*
082600060321     c     sr_selez      begsr
082700060321
082800060321     c                   if        parfil = *zeros
082900060321     c                   eval      desfil = 'Tutti'
083000060321     c                   else
083100060321     c     parfil        chain     azorg01l
083200060321     c                   if        %found(azorg01l)
083300060321     c                   eval      desfil = orgdes
083400060321     c                   endif
083500060321     c                   endif
083600060321
083700060321     c                   eval      parcar01 = parcar(01)
083800060321     c                   eval      parcar02 = parcar(02)
083900060321     c                   eval      parcar03 = parcar(03)
084000060321     c                   eval      parcar04 = parcar(04)
084100060321     c                   eval      parcar05 = parcar(05)
084200060321     c                   eval      parcar06 = parcar(06)
084300060321     c                   eval      parcar07 = parcar(07)
084400060321     c                   eval      parcar08 = parcar(08)
084500060321     c                   eval      parcar09 = parcar(09)
084600060321     c                   eval      parcar10 = parcar(10)
084700060321
084800060321     c                   write     ta58s1
084900060321
085000060321     c                   endsr
085100060324
085200060324      *--------------------------------------------------------------*
085300060324      * CONTROLLO SE IL RECORD È DA STAMPARE
085400060324      *--------------------------------------------------------------*
085500060324     c     sr_ctrrec     begsr
085600060324
085700060324     c                   eval      wok = *on
085800060324
085900060324     c                   select
086000060324      * sottoscrizione AC BASE
086100060324     c                   when      parsc01 = 'X' and tacacb = '1'
086200060324     c                   eval      wok = *off
086300060324      * rinuncia AC BASE
086400060324     c                   when      parsc02 = 'X' and tacfmp = '1'
086500060324     c                   eval      wok = *off
086600060324      * nessuna risposta
086700060324     c                   when      parsc03 = 'X' and tacnorisp = '1'
086800060324     c                   eval      wok = *off
086900060324      * discordanza
087000060324     c                   when      parsc04 = 'X' and tacdisc = '1'
087100060324     c                   eval      wok = *off
087200060324     c                   endsl
087300060324
087400060324     c                   endsr
087500060320
087600060320      *--------------------------------------------------------------*
087700060320      * ROUTINE INIZIALE
087800060320      *--------------------------------------------------------------*
087900060320     c     *inzsr        begsr
088000060320
088100060320     c     *entry        plist
088200060320     c                   parm                    kpjba
088300060320     c                   movel     kpjbu         parm01
088400060320
088500060320     c     *dtaara       define    §azute        azuteds
088600060320     c     *dtaara       define    §datiute      ddatiute
088700060320
088800060320     c                   in(e)     *dtaara
088900060320     c                   if        %error or rsut = *blanks
089000060320     c                   clear                   tibs34ds
089100060320     c                   call      'TIBS34R'
089200060320     c                   parm                    tibs34ds
089300060320     c                   in        *dtaara
089400060320     c                   endif
089500060320
089600060320
089700060320      * se s.i. di prova imposto la libreria di prova
089800060320     c                   if        %subst(knsif:7:1) = 'P'
089900060320     c                   eval      wlib = 'GAITRAAZP '
090000060320     c                   else
090100060320     c                   eval      wlib = 'GAITRAAZM '
090200060320     c                   endif
090300060320
090400060320      * se richiesta la generazione del file lo pulisco
090500060321     c                   if        parfile <> 'S'
090600060320     c                   clear                   comman
090700060320     c                   movel(p)  cmd(1)        comman
090800060320     c                   eval      %subst(comman:13:9) = wlib
090900060320     c                   call      'QCMDEXC'
091000060320     c                   parm                    comman
091100060320     c                   parm                    lenght
091200060320     c                   endif
091300060320
091400060320      * duplico il file di comodo in qtemp
091500060320     c                   clear                   comman
091600060320     c                   movel(p)  cmd(2)        comman
091700060320     c                   eval      %subst(comman:32:9) = wlib
091800060320     c                   call      'QCMDEXC'
091900060320     c                   parm                    comman
092000060320     c                   parm                    lenght
092100060320
092200060320      * apro il file di comodo per la stampa
092300060320     c                   open      wftac10f
092400060322      * se ordinata per codice apro la vista logica x ksc
092500060322     c                   if        parord = 'C'
092600060322     c                   open      wftac11l
092700060323     c                   eval      *in01 = *off
092800060322     c                   else
092900060322      * se ordinata per ragione sociale apro vista logica x rag
093000060322     c                   open      wftac12l
093100060323     c                   eval      *in01 = *on
093200060322     c                   endif
093300060320
093400060320      * se richiesta la creazione del file lo apro
093500060321     c                   if        parfile <> 'S'
093600060320     c                   open      wftac00f
093700060320     c                   endif
093800060320
093900060320      * reperisco data e ora del lancio
094000060320     c                   time                    w0140
094100060320     c                   move      w0140         wdata
094200060320     c                   movel     w0140         wora
094300060320
094400060320      * carico in sk i p.o. PPT - DPD - EEX
094500060320     c                   clear                   sknocl
094600060320     c     *loval        setll     azorg01l
094700060320     c                   do        *hival
094800060320     c                   read      azorg01l
094900060320      * fine file
095000060320     c                   if        %eof(azorg01l)
095100060320     c                   leave
095200060320     c                   endif
095300060320      * escludo i p.o. non validi
095400060320     c                   if        orgfva <> *blanks or
095500060320     c                             (orgfag <> 'A' and orgfag <> 'F')
095600060320     c                   iter
095700060320     c                   endif
095800060320     c                   eval      og143 = orgde3
095900060323      * escludo i p.o. non PPT - DPD - EEX - FED
096000060321     c                   if        §ogntw <> 'PPT' and §ogntw <> 'DPD' and
096100060323     c                             §ogntw <> 'EEX' and §ogntw <> 'FED'
096200060320     c                   iter
096300060320     c                   endif
096400060320     c     orgfil        lookup    sknocl                                 30
096500060320     c                   if        not *in30
096600060320     c                   eval      yy = 1
096700060320     c     *zeros        lookup    sknocl(yy)                             30
096800060320     c                   if        *in30
096900060320     c                   eval      sknocl(yy) = orgfil
097000060320     c                   endif
097100060320     c                   endif
097200060320     c                   enddo
097300060320
097400060320      * pulisco la sk dei p.o. da elaborare
097500060320     c                   clear                   skpoel
097600060320
097700060320      * se ho le aree carico una sk con tutti i p.o. relativi alle aree
097800060320      * richieste
097900060321    1c                   if        parcar(01) <> *blanks
098000060320    2c                   do        10            xx
098100060321    3c                   if        parcar(xx) <> *blanks
098200060321     c                   move      parcar(xx)    kcar
098300060320     c     kazorg        setll     azorg02l
098400060320    4c                   do        *hival
098500060320     c     kazorg        reade     azorg02l
098600060320     c                   if        %eof(azorg02l)
098700060320     c                   leave
098800060320     c                   endif
098900060320     c                   if        orgfva <> *blanks or
099000060320     c                             (orgfag <> 'A' and orgfag <> 'F')
099100060320     c                   iter
099200060320     c                   endif
099300060320     c                   move      orgfil        w003a
099400060320     c     w003a         lookup    skpoel                                 30
099500060320     c                   if        not *in30
099600060320     c                   eval      yy = 1
099700060320     c     *blanks       lookup    skpoel(yy)                             30
099800060320     c                   if        *in30
099900060320     c                   eval      skpoel(yy) = w003a
100000060320     c                   endif
100100060320     c                   endif
100200060320    4c                   enddo
100300060320    3c                   endif
100400060320    2c                   enddo
100500060320
100600060320   x1c                   else
100700060320      * carico il p.o. richiesto a video
100800060321    2c                   if        parfil <>  *zeros
100900060321     c                   move      parfil        skpoel(1)
101000060320   x2c                   else
101100060320     c                   clear                   trul31ds
101200060321     c                   eval      i31abi = parabi
101300060320     c                   eval      i31cdi = dutdis
101400060320     c                   eval      i31car = dutare
101500060320     c                   eval      i31cpo = dutpou
101600060320     c                   call      'TRUL31R'
101700060320     c                   parm                    kpjba
101800060320     c                   parm                    trul31ds
101900060320     c                   if        o31pog > *zeros
102000060320     c                   movea     o31pog        skpoel
102100060320     c                   endif
102200060320     c                   endif
102300060320     c                   endif
102400060321
102500060321      * abilitazione distretto
102600060322     c                   if        parabi = 'DI' and parcar(01) <> *blanks
102700060321     c                   eval      *in46 = *on
102800060321     c                   endif
102900060321
103000060321     c                   eval      *in12 = (simfel = *zeros)
103100060320
103200060320     c     kazorg        klist
103300060320     c                   kfld                    dutdis
103400060320     c                   kfld                    kcar
103500060320
103600060320     c     ktpt          klist
103700060320     c                   kfld                    tamksc
103800060320     c                   kfld                    tamctr
103900060320     c                   kfld                    tamprg
104000060320     c                   kfld                    kftc
104100060320
104200060320     c     kcli          klist
104300060320     c                   kfld                    kkut
104400060320     c                   kfld                    kkcc
104500060320     c                   kfld                    tamksc
104600060320
104700060320     c     ktabel        klist
104800060320     c                   kfld                    codut
104900060320     c                   kfld                    kcod
105000060320     c                   kfld                    kkey
105100060320
105200060320     c                   endsr
105300060320      *--------------------------------------------------------------*
105400060320**   cmd - comando
105500060320CLRPFM FILE(         /WFTAC00F)
105600060320CRTDUPOBJ OBJ(WFTAC1*) FROMLIB(         ) OBJTYPE(*FILE) TOLIB(QTEMP)
