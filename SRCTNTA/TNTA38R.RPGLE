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
005100111122     d wmind           s                   like(tpdmin)
005200060215     d wksc            s                   like(tamksc)
005300060320     d wlib            s              9
005400060324     d wok             s              1    inz(*off)
005500060320     d wora            s              6  0
005600060215     d wtptapl         s                   like(tptapl)
005700060215     d wtpttma         s                   like(tpttma)
005800060320     d w003a           s              3
005900060320     d w0030           s              3  0
006000060320     d w0140           s             14  0
006100060320     d xx              s              3  0
006200060320     d yy              s              3  0
006300060320
006400060320      *------------------------------------------------------------------------*
006500060320      *   S C H I E R E
006600060320      *------------------------------------------------------------------------*
006700060320     d cmd             s             80    dim(2) ctdata perrcd(1)
006800060320     d sknocl          s              3  0 dim(250)
006900060320     d skpoel          s              3    dim(250)
007000060215
007100060215      *------------------------------------------------------------------------*
007200060215      *   D S   I N T E R N E / E S T E R N E
007300060215      *------------------------------------------------------------------------*
007400060320     d dswftac0      e ds                  extname(wftac00f)
007500060320     d                                     prefix(f_)
007600060320     d                                     inz
007700060320     d dswftac1      e ds                  extname(wftac10f)
007800060320     d                                     inz
007900060320
008000060320     d parm01          ds
008100060321     d  parfil                 1      3  0
008200060321     d  parcar                 4     33    dim(10)
008300060321     d  parsc01               34     34
008400060321     d  parsc02               35     35
008500060321     d  parsc03               36     36
008600060321     d  parsc04               37     37
008700060323     d  parfile               38     38
008800060323     d  parord                39     39
008900060323     d  parabi                40     41
009000060321
009100060321     d wlbdat          ds                  inz
009200060321     d  g02dat                 1      8  0
009300060321     d  g02inv                 9     16  0
009400060321     d  g02err                17     17
009500060321     d  g02tgi                18     22  0
009600060320
009700060320     d azuteds       e ds                  extname(azute00f)
009800060320     d ddatiute      e ds
009900060726     d dfab          e ds
010000060320     d ds_cnaco      e ds                  inz  extname(Cnaco00f)
010100060320     d ds_cnind      e ds                  inz  extname(Cnind00f)
010200060320     d ds_cnclp      e ds                  inz  extname(Cnclp00f)
010300060320     d ds_fncls      e ds                  inz  extname(Fncls00f)
010400060215     d kpjba         e ds
010500060215     d dsta01        e ds
010600060320     d og143         e ds
010700060726     d tibs02ds      e ds
010800060320     d tibs34ds      e ds
010900060320     d tibs69ds      e ds
011000060320     d trul31ds      e ds                  inz
011100060215
011200060215      *------------------------------------------------------------------------*
011300060215
011400060320      * ciclo per i p.o. caricati in schiera
011500060320    1c                   do        250           xx
011600060320     c                   if        skpoel(xx) = *blanks
011700060320     c                   leave
011800060320     c                   endif
011900060320
012000060320     c                   clear                   wksc
012100060215     c                   eval      wctr = 999
012200060320     c                   move      *all'0'       kksc
012300060320     c                   movel     skpoel(xx)    kksc
012400060215
012500060320     c     kksc          setll     tntam04l
012600060320    2c                   do        *hival
012700060215     c                   read      tntam04l
012800060215      * fine file
012900060215     c                   if        %eof(tntam04l)
013000060215     c                   leave
013100060215     c                   endif
013200060320
013300060320      * cambio tariffa
013400060320    3c                   if        tamctr <> wctr or tamksc <> wksc
013500060320
013600060320     c                   movel     tamflo        dsta01
013700060320
013800060320      * se p.o. maggiore esco
013900060320     c                   movel     tamksc        w003a
014000060320     c                   if        w003a > skpoel(xx)
014100060320     c                   leave
014200060320     c                   endif
014300060320      * escludo le annullate
014400060215     c                   if        tamatb <> *blanks
014500060215     c                   iter
014600060215     c                   endif
014700060320      * escludo le bloccate
014800060320     c                   if        tambap = 'B'
014900060320     c                   iter
015000060320     c                   endif
015100060320      * escludo le tariffe EEX - DPD - FED
015200060320     c                   if        tamfie = 'E' or §tadpd = 'S' or
015300060320     c                             §tafed = 'S'
015400060320     c                   iter
015500060320     c                   endif
015600060320
015700060320      * cerco i dati anagrafici
015800060320     c                   clear                   tibs69ds
015900060320     c                   move      tamksc        i69kac
016000060320     c                   move      tamksc        i69kin
016100060320     c                   move      tamksc        i69kcp
016200060320     c                   call      'TIBS69R'
016300060320     c                   parm                    tibs69ds
016400060320     c                   parm                    ds_cnaco
016500060320     c                   parm                    ds_cnind
016600060320     c                   parm                    ds_cnclp
016700060320     c                   parm                    ds_fncls
016800060320      * escludo le tariffe dei clienti bloccati
016900130315     c                   if        acoabl <> ' '
017000060320     c                   iter
017100060320     c                   endif
017200060323      * escludo le tariffe dei clienti PPT - DPD - EEX - FED
017300060320     c                   movel     tamksc        w0030
017400060320     c     w0030         lookup    sknocl                                 30
017500060320     c                   if        *in30
017600060320     c                   iter
017700060320     c                   endif
017800060320      * escludo le tariffe dei clienti che non hanno avuto spedizioni fatturate
017900060320      * dopo il 1/1/2005
018000060321     c                   if        clpdus > *zeros
018100060321      * controllo la data lunga 8
018200060321     c                   clear                   wlbdat
018300060321     c                   z-add     clpdus        g02inv
018400060321     c                   eval      g02err = '3'
018500060321     c                   call      'XSRDA8'
018600060321     c                   parm                    wlbdat
018700060321     c                   if        g02inv < 20050101
018800060321     c                   iter
018900060321     c                   endif
019000060321     c                   endif
019100060320
019200060320      * cerco i dati delle tariffe particolari
019300060320     c                   eval      wacbase = *off
019400060320     c                   clear                   witrd
019500111122     c                   clear                   wmind
019600060320     c                   eval      wacplus = *off
019700060323     c                   eval      wacplusc = *off
019800060320     c                   clear                   wtpttma
019900060320     c                   clear                   wtptapl
020000060320     c                   eval      wacplusr = *off
020100060320
020200060320      * controllo se AC BASE presente
020300060320     c                   eval      kftc = 'd '
020400060320     c     ktpt          setll     titpt01l
020500060320    4c                   do        *hival
020600060320     c     ktpt          reade     titpt01l
020700060320      * fine file
020800060320     c                   if        %eof(titpt01l)
020900060320     c                   leave
021000060320     c                   endif
021100060320      * non annullate
021200060320     c                   if        tptatb <> *blanks
021300060320     c                   iter
021400060320     c                   endif
021500060320     c                   eval      wacbase = *on
021600060320      * cerco importo AC BASE
021700060320     c     ktpt          setll     titpd01l
021800060320    5c                   do        *hival
021900060320     c     ktpt          reade     titpd01l
022000060320      * fine file
022100060320     c                   if        %eof(titpd01l)
022200060320     c                   leave
022300060320     c                   endif
022400060320      * non annullate
022500060320     c                   if        tpdatb <> *blanks
022600060320     c                   iter
022700060320     c                   endif
022800060320     c                   eval      witrd = tpditr
022900111122     c                   eval      wmind = tpdmin
023000060320     c                   leave
023100060320    5c                   enddo
023200060320    4c                   enddo
023300060320
023400060320      * controllo se AC PLUS presente
023500060321     c                   eval      kftc = 'C '
023600060320     c     ktpt          setll     titpt01l
023700060320    4c                   do        *hival
023800060320     c     ktpt          reade     titpt01l
023900060320     c                   if        %eof(titpt01l)
024000060320     c                   leave
024100060320     c                   endif
024200060320      * non annullate
024300060320     c                   if        tptatb <> *blanks
024400060320     c                   iter
024500060320     c                   endif
024600060323     c                   eval      wacplus = *on
024700060320      * AC PLUS reale/fittizia
024800060320     c                   eval      wtpttma = tpttma
024900060320      * AC PLUS par/arr/entrambe
025000060320     c                   eval      wtptapl = tptapl
025100060320      * controllo se AC PLUS reale e completa
025200060320     c                   if        tpttma = *blanks
025300060320     c                   select
025400060321     c                   when      tptapl = *blanks
025500060323     c                   eval      wacplusc = *on
025600060322     c                   when      tambap = 'P' and tptapl = 'P'
025700060323     c                   eval      wacplusc = *on
025800060322     c                   when      tambap = 'A' and tptapl = 'A'
025900060323     c                   eval      wacplusc = *on
026000060320     c                   endsl
026100060320     c                   leave
026200060321     c                   endif
026300060320    4c                   enddo
026400060320
026500060324      * scrivo il file solo se:
026600060320     c                   select
026700060320      * sottoscrizione AC BASE
026800060323     c                   when      wacbase = *on
026900060320     c                   exsr      scrivi
027000060320      * rinuncia AC BASE
027100060323     c                   when      tamfmp = 'S'
027200060320     c                   exsr      scrivi
027300060320      * nessuna risposta
027400060323     c                   when      tamfmp = *blanks and
027500060323     c                             wacbase = *off and wacplusc = *off
027600060320     c                   exsr      scrivi
027700060323      * AC PLUS inserita
027800060323     c                   when      wacplus = *on
027900060323     c                   exsr      scrivi
028000060323     c                   endsl
028100060215
028200060320     c                   eval      wksc = tamksc
028300060320     c                   eval      wctr = tamctr
028400060215
028500060215      * se la tariffa che sto leggendo deve ancora andare in vigore la elaboro
028600060215      * poi però devo elaborare anche la tariffa precedente che è in vigore ora
028700060215     c                   if        tamddt > *date
028800060215     c                   eval      wctr = 999
028900060215     c                   endif
029000060320    3c                   endif
029100060320
029200060320    2c                   enddo
029300060215
029400060320    1c                   enddo
029500060320
029600060320      * stampo
029700060323     c                   if        parfile <> 'F'
029800060323     c   02              exsr      sr_stampa
029900060323     c  n02              exsr      sr_selez
030000060323     c  n02              write     ta58e1
030100060321     c                   endif
030200060215
030300060215     c                   eval      *inlr = *on
030400060215
030500060215      *------------------------------------------------------------------------*
030600060320      * SCRIVO IL FILE PER LA STAMPA
030700060215      *------------------------------------------------------------------------*
030800060215     c     scrivi        begsr
030900060215
031000060323     c  n02              eval      *in02 = *on
031100060323
031200060320     c                   clear                   wftac10
031300060320
031400060320     c                   movel     tamksc        w0030
031500060320     c     w0030         chain     azorg01l
031600060215
031700060320      * utente/data/ora elaborazione
031800060320     c                   eval      tacpru = knmus
031900060321     c                   eval      tacdte = wdata
032000060321     c                   eval      tacore = wora
032100060320      * distretto
032200060320     c                   eval      tacdis = orgfl3
032300060320     c                   eval      kcod = '17'
032400060320     c                   eval      kkey = tacdis
032500060320     c     ktabel        chain     tabel00f
032600060320     c                   if        not %found(tabel00f) or tblflg <> *blanks
032700060320     c                   eval      tacddis = *all'*'
032800060320     c                   else
032900060320     c                   eval      tacddis = tbluni
033000060320     c                   endif
033100060320      * area
033200060321     c                   eval      taccar = orgcar
033300060320     c                   eval      kcod = '05'
033400060321     c                   movel     taccar        kkey
033500060320     c     ktabel        chain     tabel00f
033600060320     c                   if        not %found(tabel00f) or tblflg <> *blanks
033700060321     c                   eval      tacdcar = *all'*'
033800060320     c                   else
033900060321     c                   eval      tacdcar = tbluni
034000060320     c                   endif
034100060320      * p.o.
034200060320     c                   eval      tacfil = orgfil
034300060320     c                   if        not %found(azorg01l) or orgfva <> *blanks
034400060321     c                   eval      tacdfil = *all'*'
034500060320     c                   else
034600060320     c                   eval      tacdfil = orgdes
034700060320     c                   endif
034800060320      * commerciale
034900060320     c                   eval      taccmm = clpage
035000060320     c                   eval      kcod = '01'
035100060320     c                   movel     taccmm        kkey
035200060320     c     ktabel        chain     tabel00f
035300060320     c                   if        not %found(tabel00f) or tblflg <> *blanks
035400060320     c                   eval      tacdcmm = *all'*'
035500060320     c                   else
035600060320     c                   eval      tacdcmm = tbluni
035700060320     c                   endif
035800060215      * cliente
035900060320     c                   eval      tacksc = tamksc
036000060320     c                   eval      tacrag = acorag
036100060320      * indirizzo
036200060320     c                   eval      tacvia = indvia
036300060320     c                   eval      taccit = indcit
036400060322     c                   eval      taccap = indcae
036500060320     c                   eval      tacprv = indprv
036600060320     c                   eval      tacnaz = indsta
036700060321      * telefono
036800060321     c                   eval      tactel = indtel
036900060215      * partita IVA/codice fiscale
037000060215     c                   if        indiva <> *Blanks
037100060320     c                   eval      tacivacf = indiva
037200060215     c                   else
037300060215     c                   if        indcdf <> *Blanks
037400060320     c                   eval      tacivacf = indcdf
037500060215     c                   endif
037600060215     c                   endif
037700060320      * importanza cliente
037800060320     c                   eval      tacclv = clpclv
037900060215      * data ultima spedizione fattura
038000060320     c                   eval      tacdus = clpdus
038100060215      * tariffa
038200060320     c                   eval      tacctr = tamctr
038300060320     c                   eval      tacprg = tamprg
038400060320     c                   eval      tacddt = tamddt
038500060320     c                   eval      tacdst = tamdst
038600060323      * tariffa bloccata partenza/arrivo
038700060322     c                   if        tambap = *blanks
038800060322     c                   eval      tacbap = 'E'
038900060215     c                   else
039000060322     c                   eval      tacbap = tambap
039100060215     c                   endif
039200060320      * rifiuta AC BASE
039300060320     c                   if        tamfmp = 'S'
039400060320     c                   eval      tacfmp = '1'
039500060320     c                   else
039600060320     c                   eval      tacfmp = '0'
039700060320     c                   endif
039800060320      * presente AC BASE
039900060320     c                   if        wacbase = *on
040000060320     c                   eval      tacacb = '1'
040100060320     c                   else
040200060320     c                   eval      tacacb = '0'
040300060320     c                   endif
040400060323      * presente AC PLUS
040500060323     c                   if        wacplus = *on
040600060323      * completa
040700060323     c                   if        wacplusc = *on
040800060320     c                   eval      tacacp = '1'
040900060323     c                   else
041000060323     c                   eval      tacacp = '0'
041100060323     c                   endif
041200060321      * AC PLUS reale/fittizia
041300060321     c                   if        wtpttma = *blanks
041400060321     c                   eval      tactmaacp = 'R'
041500060321     c                   else
041600060321     c                   eval      tactmaacp = 'F'
041700060321     c                   endif
041800060321      * AC PLUS par/arr/entr.
041900060321     c                   if        wtptapl = *blanks
042000060321     c                   eval      tacaplacp = 'E'
042100060321     c                   else
042200060321     c                   eval      tacaplacp = wtptapl
042300060321     c                   endif
042400060324     c                   else
042500060324     c                   eval      tacacp = '0'
042600060320     c                   endif
042700060320      * nessuna risposta
042800060323     c                   if        wacbase = *off and wacplusc = *off and
042900060320     c                             tamfmp = *blanks
043000060323     c                   eval      tacnorisp = '1'
043100060320     c                   else
043200060323     c                   eval      tacnorisp = '0'
043300060320     c                   endif
043400060726
043500060320      * discordanza AC BASE con CLV
043600060321     c                   if        wacbase = *on
043700060726      * per prima cosa controllo se il cliente è presente nella tabella della
043800060726      * forzatura AC BASE in modo da fare il confronto con quanto inserito
043900060726      * in tabella
044000060726     c                   clear                   tibs02ds
044100060726     c                   clear                   dfab
044200060726     c                   eval      t02mod = 'C'
044300060726     c                   eval      t02sif = knsif
044400060726     c                   eval      t02cod = 'FAB'
044500060726     c                   movel     tamksc        t02ke1
044600060726     c                   call      'TIBS02R'
044700060726     c                   parm                    kpjba
044800060726     c                   parm                    tibs02ds
044900060726      * se lo trovo faccio il confronto e non faccio il confronto con CLV
045000060726     c                   if        t02err = *blanks
045100060726     c                   eval      dfab = t02uni
045200060726     c                   if        d§fabitr > 0 and witrd < d§fabitr
045300060726     c                   eval      tacdisc = '1'
045400060726     c                   else
045500060726     c                   eval      tacdisc = '0'
045600060726     c                   endif
045700060726     c                   goto      noclv
045800060726     c                   endif
045900060726      * controllo con CLV
046000060320     c                   select
046100060320      * clienti Top o Direzionali
046200060320     c                   when      (clpclv = 'T' or clpclv = 'D') and
046300060323     c                             witrd < 0,002
046400060320     c                   eval      tacdisc = '1'
046500060320      * clienti A
046600060323     c                   when      clpclv = 'A' and witrd < 0,003
046700060320     c                   eval      tacdisc = '1'
046800060320      * clienti C o B
046900060320     c                   when      (clpclv = 'C' or clpclv = 'B') and
047000060323     c                             witrd < 0,005
047100060320     c                   eval      tacdisc = '1'
047200060320     c                   other
047300060320     c                   eval      tacdisc = '0'
047400060320     c                   endsl
047500111122      * presente minimo oppure no
047600111122     c                   if        wmind = 0
047700111122     c                   eval      tacac8 = '1'
047800111122     c                   else
047900111122     c                   eval      tacac8 = '0'
048000111122     c                   endif
048100111122     c                   endif
048200060726     c     noclv         tag
048300060726
048400060320      * importo AC BASE
048500060320     c                   eval      tacitracb = witrd
048600060215      * scrivo file
048700060320     c                   write     wftac10
048800060320
048900060320      * se richiesto genero il file di work
049000060321     c                   if        parfile <> 'S'
049100060320     c                   clear                   wftac000
049200060320     c                   eval      dswftac0 = dswftac1
049300060320     c                   write     wftac000
049400060320     c                   endif
049500060215
049600060215     c                   endsr
049700060320
049800060320      *--------------------------------------------------------------*
049900060320      * STAMPO
050000060320      *--------------------------------------------------------------*
050100060320     c     sr_stampa     begsr
050200060321
050300060321      * stampo la pagina con le selezioni
050400060321     c                   exsr      sr_selez
050500060320
050600060320      * leggo file di comodo per la stampa
050700060323     c  n01*loval        setll     wftac11l
050800060323     c   01*loval        setll     wftac12l
050900060321     c                   do        *hival
051000060323     c  n01              read      wftac11l
051100060323     c   01              read      wftac12l
051200060322     c                   if        %eof
051300060320     c                   leave
051400060321     c                   endif
051500060324      * controllo se è da stampare in base alle selezioni fatte
051600060324     c                   exsr      sr_ctrrec
051700060324     c                   if        wok = *on
051800060324     c                   iter
051900060324     c                   endif
052000060321
052100060320      * rottura per distretto
052200060321     c                   if        pdis <> tacdis
052300060321      * totali per distretto
052400060322     c                   if        pdis <> *blanks and *in12
052500060324      * prima stampo il totale del p.o.
052600060322     c                   write     ta58tf1
052700060322     c                   clear                   ptfksc
052800060322     c                   clear                   ptfctr
052900060322     c                   clear                   ptffmp
053000060322     c                   clear                   ptfacb
053100060322     c                   clear                   ptfacp
053200060322     c                   clear                   ptfrisp
053300060322     c                   clear                   ptfdisc
053400060324      * salto pagina
053500060324     c                   write     ta58t1
053600060324     c                   write     ta58t2
053700060324     c                   write     ta58t3
053800060324      * poi stampo il totale area
053900060322     c                   write     ta58tc1
054000060322     c                   clear                   ptcksc
054100060322     c                   clear                   ptcctr
054200060322     c                   clear                   ptcfmp
054300060322     c                   clear                   ptcacb
054400060322     c                   clear                   ptcacp
054500060322     c                   clear                   ptcrisp
054600060322     c                   clear                   ptcdisc
054700060324      * salto pagina
054800060324     c                   write     ta58t1
054900060324     c                   write     ta58t2
055000060324     c                   write     ta58t3
055100060324      * infine stampo il totale del distretto
055200060321     c                   write     ta58td1
055300060321     c                   clear                   ptdksc
055400060321     c                   clear                   ptdctr
055500060321     c                   clear                   ptdfmp
055600060321     c                   clear                   ptdacb
055700060321     c                   clear                   ptdacp
055800060321     c                   clear                   ptdrisp
055900060321     c                   clear                   ptddisc
056000060321     c                   endif
056100060324      * stampo la testata
056200060321     c                   eval      pdis = tacdis
056300060321     c                   eval      pdesdis = tacddis
056400060321     c                   eval      pcar = taccar
056500060321     c                   eval      pdescar = tacdcar
056600060321     c                   eval      pfil = tacfil
056700060321     c                   eval      pdesfil = tacdfil
056800060321     c                   eval      pcmm = taccmm
056900060321     c                   eval      pdescmm = tacdcmm
057000060321     c                   write     ta58t1
057100060321     c                   write     ta58t2
057200060321     c                   write     ta58t3
057300060321     c                   write     ta58t4
057400060321     c                   endif
057500060320      * rottura per area
057600060322     c                   if        pcar <> taccar
057700060321      * totali per area
057800060323     c                   if        pcar <> *zeros and (*in46 or *in12)
057900060324      * prima stampo il totale del p.o.
058000060322     c                   write     ta58tf1
058100060322     c                   clear                   ptfksc
058200060322     c                   clear                   ptfctr
058300060322     c                   clear                   ptffmp
058400060322     c                   clear                   ptfacb
058500060322     c                   clear                   ptfacp
058600060322     c                   clear                   ptfrisp
058700060322     c                   clear                   ptfdisc
058800060324      * salto pagina
058900060324     c                   write     ta58t1
059000060324     c                   write     ta58t2
059100060324     c                   write     ta58t3
059200060324      * poi stampo il totale area
059300060321     c                   write     ta58tc1
059400060321     c                   clear                   ptcksc
059500060321     c                   clear                   ptcctr
059600060321     c                   clear                   ptcfmp
059700060321     c                   clear                   ptcacb
059800060321     c                   clear                   ptcacp
059900060321     c                   clear                   ptcrisp
060000060321     c                   clear                   ptcdisc
060100060321     c                   endif
060200060324      * stampo la testata
060300060321     c                   eval      pdis = tacdis
060400060321     c                   eval      pdesdis = tacddis
060500060321     c                   eval      pcar = taccar
060600060321     c                   eval      pdescar = tacdcar
060700060321     c                   eval      pfil = tacfil
060800060321     c                   eval      pdesfil = tacdfil
060900060321     c                   eval      pcmm = taccmm
061000060321     c                   eval      pdescmm = tacdcmm
061100060321     c                   write     ta58t1
061200060321     c                   write     ta58t2
061300060321     c                   write     ta58t3
061400060321     c                   write     ta58t4
061500060321     c                   endif
061600060320      * rottura per p.o.
061700060321     c                   if        pfil <> tacfil
061800060324      * totali per p.o.
061900060321     c                   if        pfil <> *zeros
062000060321     c                   write     ta58tf1
062100060321     c                   clear                   ptfksc
062200060321     c                   clear                   ptfctr
062300060321     c                   clear                   ptffmp
062400060321     c                   clear                   ptfacb
062500060321     c                   clear                   ptfacp
062600060321     c                   clear                   ptfrisp
062700060321     c                   clear                   ptfdisc
062800060321     c                   endif
062900060321      * testata
063000060321     c                   eval      pdis = tacdis
063100060321     c                   eval      pdesdis = tacddis
063200060321     c                   eval      pcar = taccar
063300060321     c                   eval      pdescar = tacdcar
063400060321     c                   eval      pfil = tacfil
063500060321     c                   eval      pdesfil = tacdfil
063600060321     c                   eval      pcmm = taccmm
063700060321     c                   eval      pdescmm = tacdcmm
063800060321     c                   write     ta58t1
063900060321     c                   write     ta58t2
064000060321     c                   write     ta58t3
064100060321     c                   write     ta58t4
064200060320     c                   endif
064300060321
064400060321      * rottura per commerciale
064500060321     c                   if        pcmm <> taccmm
064600060321     c                   eval      pdis = tacdis
064700060321     c                   eval      pdesdis = tacddis
064800060321     c                   eval      pcar = taccar
064900060321     c                   eval      pdescar = tacdcar
065000060321     c                   eval      pfil = tacfil
065100060321     c                   eval      pdesfil = tacdfil
065200060321     c                   eval      pcmm = taccmm
065300060321     c                   eval      pdescmm = tacdcmm
065400060321      * salto pagina
065500060321     c                   if        *in90 = *on
065600060321     c                   write     ta58t1
065700060321     c                   write     ta58t2
065800060321     c                   write     ta58t3
065900060321     c                   write     ta58t4
066000060321     c                   eval      *in90 = *off
066100060321     c                   else
066200060322     c                   write     ta58t4
066300060321     c                   endif
066400060321     c                   endif
066500060321
066600060321      * conto quanti clienti
066700060321     c                   if        savksc <> tacksc
066800060321     c                   add       1             tksc
066900060321     c                   add       1             ptdksc
067000060321     c                   add       1             ptcksc
067100060321     c                   add       1             ptfksc
067200060321     c                   eval      savksc = tacksc
067300060321     c                   endif
067400060321      * conto quante tariffe
067500060321     c                   add       1             tctr
067600060321     c                   add       1             ptdctr
067700060321     c                   add       1             ptcctr
067800060321     c                   add       1             ptfctr
067900060321
068000060321      * ragione sociale
068100060321     c                   eval      prag = tacrag
068200060321      * validità tariffa
068300060322     c                   clear                   pbap
068400060321     c                   select
068500060322     c                   when      tacbap = 'P'
068600060322     c                   eval      pbap = 'PAR'
068700060322     c                   when      tacbap = 'A'
068800060322     c                   eval      pbap = 'ARR'
068900060322     c                   when      tacbap = 'E'
069000060323     c                   eval      pbap = 'P+A'
069100060321     c                   endsl
069200060321      * rifiuta AC BASE
069300060321     c                   clear                   pfmp
069400060321     c                   if        tacfmp = '0'
069500060321     c                   eval      pfmp = 'NO'
069600060321     c                   endif
069700060321     c                   if        tacfmp = '1'
069800060321     c                   eval      pfmp = 'SI'
069900060321     c                   add       1             tfmp
070000060321     c                   add       1             ptdfmp
070100060321     c                   add       1             ptcfmp
070200060321     c                   add       1             ptffmp
070300060321     c                   endif
070400060321      * presente AC BASE
070500060321     c                   clear                   pacb
070600060321     c                   if        tacacb = '0'
070700060321     c                   eval      pacb = 'NO'
070800060321     c                   endif
070900060321     c                   if        tacacb = '1'
071000060321     c                   eval      pacb = 'SI'
071100060321     c                   add       1             tacb
071200060321     c                   add       1             ptdacb
071300060321     c                   add       1             ptcacb
071400060321     c                   add       1             ptfacb
071500060321     c                   endif
071600060324      * presente AC PLUS completa
071700060321     c                   clear                   pacp
071800060321     c                   if        tacacp = '0'
071900060321     c                   eval      pacp = 'NO'
072000060321     c                   endif
072100060321     c                   if        tacacp = '1'
072200060321     c                   eval      pacp = 'SI'
072300060321     c                   add       1             tacp
072400060321     c                   add       1             ptdacp
072500060321     c                   add       1             ptcacp
072600060321     c                   add       1             ptfacp
072700060321     c                   endif
072800060321      * validità AC PLUS
072900060321     c                   clear                   papl
073000060324     c                   if        tactmaacp = 'R'
073100060321     c                   select
073200060321     c                   when      tacaplacp = 'P'
073300060321     c                   eval      papl = 'PAR'
073400060321     c                   when      tacaplacp = 'A'
073500060321     c                   eval      papl = 'ARR'
073600060321     c                   when      tacaplacp = 'E'
073700060323     c                   eval      papl = 'P+A'
073800060321     c                   endsl
073900060324     c                   endif
074000060321      * risposta
074100060321     c                   clear                   prisp
074200060323     c                   if        tacnorisp = '1'
074300060321     c                   eval      prisp = 'NO'
074400060321     c                   add       1             trisp
074500060321     c                   add       1             ptdrisp
074600060321     c                   add       1             ptcrisp
074700060321     c                   add       1             ptfrisp
074800060321     c                   endif
074900060323     c                   if        tacnorisp = '0'
075000060321     c                   eval      prisp = 'SI'
075100060321     c                   endif
075200060321      * discordanza
075300060321     c                   clear                   pdisc
075400060321     c                   if        tacdisc = '0'
075500060321     c                   eval      pdisc = 'NO'
075600060321     c                   endif
075700060321     c                   if        tacdisc = '1'
075800060321     c                   eval      pdisc = 'SI'
075900060321     c                   add       1             tdisc
076000060321     c                   add       1             ptddisc
076100060321     c                   add       1             ptcdisc
076200060321     c                   add       1             ptfdisc
076300060321     c                   endif
076400060321      * importo AC BASE
076500060321     c                   eval      pitr = tacitracb
076600060321      * salto pagina
076700060324     c                   if        *in90 = *on
076800060321     c                   write     ta58t1
076900060321     c                   write     ta58t2
077000060321     c                   write     ta58t3
077100060321     c                   write     ta58t4
077200060321     c                   eval      *in90 = *off
077300060321     c                   endif
077400060321      * stampo dettaglio
077500060321     c                   write     ta58d1
077600060320
077700060320 e1  c                   enddo
077800060321
077900060321      * totale generale
078000060324      * prima stampo il totale del p.o.
078100060322     c                   write     ta58tf1
078200060324      * salto pagina
078300060324     c                   if        *in12 or *in46
078400060324     c                   write     ta58t1
078500060324     c                   write     ta58t2
078600060324     c                   write     ta58t3
078700060324      * stampo il totale dell'area
078800060324     c                   write     ta58tc1
078900060324     c                   endif
079000060324      * salto pagina
079100060324     c                   if        *in12
079200060324     c                   write     ta58t1
079300060324     c                   write     ta58t2
079400060324     c                   write     ta58t3
079500060324      * stampo il totale del distretto
079600060324     c                   write     ta58td1
079700060324      * salto pagina
079800060324     c                   write     ta58t1
079900060324     c                   write     ta58t2
080000060324     c                   write     ta58t3
080100060324      * stampo il totale generale
080200060324     c                   write     ta58tt
080300060324     c                   endif
080400060320
080500060320      * fine stampa
080600060321     c                   write     ta58e1
080700060320
080800060320     c                   endsr
080900060321
081000060321      *--------------------------------------------------------------*
081100060321      * STAMPO LA PAGINA DELLE SELEZIONI
081200060321      *--------------------------------------------------------------*
081300060321     c     sr_selez      begsr
081400060321
081500060321     c                   if        parfil = *zeros
081600060321     c                   eval      desfil = 'Tutti'
081700060321     c                   else
081800060321     c     parfil        chain     azorg01l
081900060321     c                   if        %found(azorg01l)
082000060321     c                   eval      desfil = orgdes
082100060321     c                   endif
082200060321     c                   endif
082300060321
082400060321     c                   eval      parcar01 = parcar(01)
082500060321     c                   eval      parcar02 = parcar(02)
082600060321     c                   eval      parcar03 = parcar(03)
082700060321     c                   eval      parcar04 = parcar(04)
082800060321     c                   eval      parcar05 = parcar(05)
082900060321     c                   eval      parcar06 = parcar(06)
083000060321     c                   eval      parcar07 = parcar(07)
083100060321     c                   eval      parcar08 = parcar(08)
083200060321     c                   eval      parcar09 = parcar(09)
083300060321     c                   eval      parcar10 = parcar(10)
083400060321
083500060321     c                   write     ta58s1
083600060321
083700060321     c                   endsr
083800060324
083900060324      *--------------------------------------------------------------*
084000060324      * CONTROLLO SE IL RECORD È DA STAMPARE
084100060324      *--------------------------------------------------------------*
084200060324     c     sr_ctrrec     begsr
084300060324
084400060324     c                   eval      wok = *on
084500060324
084600060324     c                   select
084700060324      * sottoscrizione AC BASE
084800060324     c                   when      parsc01 = 'X' and tacacb = '1'
084900060324     c                   eval      wok = *off
085000060324      * rinuncia AC BASE
085100060324     c                   when      parsc02 = 'X' and tacfmp = '1'
085200060324     c                   eval      wok = *off
085300060324      * nessuna risposta
085400060324     c                   when      parsc03 = 'X' and tacnorisp = '1'
085500060324     c                   eval      wok = *off
085600060324      * discordanza
085700060324     c                   when      parsc04 = 'X' and tacdisc = '1'
085800060324     c                   eval      wok = *off
085900060324     c                   endsl
086000060324
086100060324     c                   endsr
086200060320
086300060320      *--------------------------------------------------------------*
086400060320      * ROUTINE INIZIALE
086500060320      *--------------------------------------------------------------*
086600060320     c     *inzsr        begsr
086700060320
086800060320     c     *entry        plist
086900060320     c                   parm                    kpjba
087000060320     c                   movel     kpjbu         parm01
087100060320
087200060320     c     *dtaara       define    §azute        azuteds
087300060320     c     *dtaara       define    §datiute      ddatiute
087400060320
087500060320     c                   in(e)     *dtaara
087600060320     c                   if        %error or rsut = *blanks
087700060320     c                   clear                   tibs34ds
087800060320     c                   call      'TIBS34R'
087900060320     c                   parm                    tibs34ds
088000060320     c                   in        *dtaara
088100060320     c                   endif
088200060320
088300060320
088400060320      * se s.i. di prova imposto la libreria di prova
088500060320     c                   if        %subst(knsif:7:1) = 'P'
088600060320     c                   eval      wlib = 'GAITRAAZP '
088700060320     c                   else
088800060320     c                   eval      wlib = 'GAITRAAZM '
088900060320     c                   endif
089000060320
089100060320      * se richiesta la generazione del file lo pulisco
089200060321     c                   if        parfile <> 'S'
089300060320     c                   clear                   comman
089400060320     c                   movel(p)  cmd(1)        comman
089500060320     c                   eval      %subst(comman:13:9) = wlib
089600060320     c                   call      'QCMDEXC'
089700060320     c                   parm                    comman
089800060320     c                   parm                    lenght
089900060320     c                   endif
090000060320
090100060320      * duplico il file di comodo in qtemp
090200060320     c                   clear                   comman
090300060320     c                   movel(p)  cmd(2)        comman
090400060320     c                   eval      %subst(comman:32:9) = wlib
090500060320     c                   call      'QCMDEXC'
090600060320     c                   parm                    comman
090700060320     c                   parm                    lenght
090800060320
090900060320      * apro il file di comodo per la stampa
091000060320     c                   open      wftac10f
091100060322      * se ordinata per codice apro la vista logica x ksc
091200060322     c                   if        parord = 'C'
091300060322     c                   open      wftac11l
091400060323     c                   eval      *in01 = *off
091500060322     c                   else
091600060322      * se ordinata per ragione sociale apro vista logica x rag
091700060322     c                   open      wftac12l
091800060323     c                   eval      *in01 = *on
091900060322     c                   endif
092000060320
092100060320      * se richiesta la creazione del file lo apro
092200060321     c                   if        parfile <> 'S'
092300060320     c                   open      wftac00f
092400060320     c                   endif
092500060320
092600060320      * reperisco data e ora del lancio
092700060320     c                   time                    w0140
092800060320     c                   move      w0140         wdata
092900060320     c                   movel     w0140         wora
093000060320
093100060320      * carico in sk i p.o. PPT - DPD - EEX
093200060320     c                   clear                   sknocl
093300060320     c     *loval        setll     azorg01l
093400060320     c                   do        *hival
093500060320     c                   read      azorg01l
093600060320      * fine file
093700060320     c                   if        %eof(azorg01l)
093800060320     c                   leave
093900060320     c                   endif
094000060320      * escludo i p.o. non validi
094100060320     c                   if        orgfva <> *blanks or
094200060320     c                             (orgfag <> 'A' and orgfag <> 'F')
094300060320     c                   iter
094400060320     c                   endif
094500060320     c                   eval      og143 = orgde3
094600060323      * escludo i p.o. non PPT - DPD - EEX - FED
094700060321     c                   if        §ogntw <> 'PPT' and §ogntw <> 'DPD' and
094800060323     c                             §ogntw <> 'EEX' and §ogntw <> 'FED'
094900060320     c                   iter
095000060320     c                   endif
095100060320     c     orgfil        lookup    sknocl                                 30
095200060320     c                   if        not *in30
095300060320     c                   eval      yy = 1
095400060320     c     *zeros        lookup    sknocl(yy)                             30
095500060320     c                   if        *in30
095600060320     c                   eval      sknocl(yy) = orgfil
095700060320     c                   endif
095800060320     c                   endif
095900060320     c                   enddo
096000060320
096100060320      * pulisco la sk dei p.o. da elaborare
096200060320     c                   clear                   skpoel
096300060320
096400060320      * se ho le aree carico una sk con tutti i p.o. relativi alle aree
096500060320      * richieste
096600060321    1c                   if        parcar(01) <> *blanks
096700060320    2c                   do        10            xx
096800060321    3c                   if        parcar(xx) <> *blanks
096900060321     c                   move      parcar(xx)    kcar
097000060320     c     kazorg        setll     azorg02l
097100060320    4c                   do        *hival
097200060320     c     kazorg        reade     azorg02l
097300060320     c                   if        %eof(azorg02l)
097400060320     c                   leave
097500060320     c                   endif
097600060320     c                   if        orgfva <> *blanks or
097700060320     c                             (orgfag <> 'A' and orgfag <> 'F')
097800060320     c                   iter
097900060320     c                   endif
098000060320     c                   move      orgfil        w003a
098100060320     c     w003a         lookup    skpoel                                 30
098200060320     c                   if        not *in30
098300060320     c                   eval      yy = 1
098400060320     c     *blanks       lookup    skpoel(yy)                             30
098500060320     c                   if        *in30
098600060320     c                   eval      skpoel(yy) = w003a
098700060320     c                   endif
098800060320     c                   endif
098900060320    4c                   enddo
099000060320    3c                   endif
099100060320    2c                   enddo
099200060320
099300060320   x1c                   else
099400060320      * carico il p.o. richiesto a video
099500060321    2c                   if        parfil <>  *zeros
099600060321     c                   move      parfil        skpoel(1)
099700060320   x2c                   else
099800060320     c                   clear                   trul31ds
099900060321     c                   eval      i31abi = parabi
100000060320     c                   eval      i31cdi = dutdis
100100060320     c                   eval      i31car = dutare
100200060320     c                   eval      i31cpo = dutpou
100300060320     c                   call      'TRUL31R'
100400060320     c                   parm                    kpjba
100500060320     c                   parm                    trul31ds
100600060320     c                   if        o31pog > *zeros
100700060320     c                   movea     o31pog        skpoel
100800060320     c                   endif
100900060320     c                   endif
101000060320     c                   endif
101100060321
101200060321      * abilitazione distretto
101300060322     c                   if        parabi = 'DI' and parcar(01) <> *blanks
101400060321     c                   eval      *in46 = *on
101500060321     c                   endif
101600060321
101700060321     c                   eval      *in12 = (simfel = *zeros)
101800060320
101900060320     c     kazorg        klist
102000060320     c                   kfld                    dutdis
102100060320     c                   kfld                    kcar
102200060320
102300060320     c     ktpt          klist
102400060320     c                   kfld                    tamksc
102500060320     c                   kfld                    tamctr
102600060320     c                   kfld                    tamprg
102700060320     c                   kfld                    kftc
102800060320
102900060320     c     kcli          klist
103000060320     c                   kfld                    kkut
103100060320     c                   kfld                    kkcc
103200060320     c                   kfld                    tamksc
103300060320
103400060320     c     ktabel        klist
103500060320     c                   kfld                    codut
103600060320     c                   kfld                    kcod
103700060320     c                   kfld                    kkey
103800060320
103900060320     c                   endsr
104000060320      *--------------------------------------------------------------*
104100060320**   cmd - comando
104200060320CLRPFM FILE(         /WFTAC00F)
104300060320CRTDUPOBJ OBJ(WFTAC1*) FROMLIB(         ) OBJTYPE(*FILE) TOLIB(QTEMP)
