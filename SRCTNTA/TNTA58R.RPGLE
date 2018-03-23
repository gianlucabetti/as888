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
001400130905     fAZCMM01L  if   e           k disk
001500060320     fwftac00f  o    e             disk    usropn prefix(f_)
001600060320     fwftac10f  o    e             disk    usropn rename(wftac000:wftac10)
001700060322     fwftac11l  if   e           k disk    usropn rename(wftac000:wftac11)
001800060322     fwftac12l  if   e           k disk    usropn rename(wftac000:wftac12)
001900060321     ftnta58p   o    e             printer oflind(*in90)
002000060323
002100060323      *------------------------------------------------------------------------*
002200060323      *  RIEPILOGO INDICATORI
002300060323      *------------------------------------------------------------------------*
002400060323      * 01 - stampa per ragione sociale
002500060323      * 02 - ho scritto almeno 1 rcd
002600060323      * 12 - utente di sede
002700060323      * 30 - comodo
002800060323      * 46 - stampo aree + relativi totali
002900060323      * 90 - salto pagina
003000060215
003100060215      *------------------------------------------------------------------------*
003200060215      *   V A R I A B I L I
003300060215      *------------------------------------------------------------------------*
003400060320     d comman          s             80
003500060320     d codut           s              1  0 inz(1)
003600060320     d kcar            s                   like(orgcar)
003700060320     d kcod            s                   like(tblcod)
003800060320     d kksc            s                   like(tamksc)
003900060320     d kkey            s                   like(tblkey)
004000060320     d lenght          s             15  5 inz(80)
004100060320     d kftc            s                   like(tptftc)
004200060215     d kkcc            s                   like(acokcc) inz(151)
004300060215     d kkut            s                   like(acokut) inz(1)
004400060321     d savksc          s                   like(tacksc)
004500060320     d wacbase         s              1    inz(*off)
004600060320     d wacplus         s              1    inz(*off)
004700060323     d wacplusc        s              1    inz(*off)
004800060320     d wacplusr        s              1    inz(*off)
004900060215     d wctr            s                   like(tamctr)
005000060320     d wdata           s              8  0
005100060320     d witrd           s                   like(tpditr)
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
016900130315     c                   if        acoabl <> *blanks
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
019500060320     c                   eval      wacplus = *off
019600060323     c                   eval      wacplusc = *off
019700060320     c                   clear                   wtpttma
019800060320     c                   clear                   wtptapl
019900060320     c                   eval      wacplusr = *off
020000060320
020100060320      * controllo se AC BASE presente
020200060320     c                   eval      kftc = 'd '
020300060320     c     ktpt          setll     titpt01l
020400060320    4c                   do        *hival
020500060320     c     ktpt          reade     titpt01l
020600060320      * fine file
020700060320     c                   if        %eof(titpt01l)
020800060320     c                   leave
020900060320     c                   endif
021000060320      * non annullate
021100060320     c                   if        tptatb <> *blanks
021200060320     c                   iter
021300060320     c                   endif
021400060320     c                   eval      wacbase = *on
021500060320      * cerco importo AC BASE
021600060320     c     ktpt          setll     titpd01l
021700060320    5c                   do        *hival
021800060320     c     ktpt          reade     titpd01l
021900060320      * fine file
022000060320     c                   if        %eof(titpd01l)
022100060320     c                   leave
022200060320     c                   endif
022300060320      * non annullate
022400060320     c                   if        tpdatb <> *blanks
022500060320     c                   iter
022600060320     c                   endif
022700060320     c                   eval      witrd = tpditr
022800060320     c                   leave
022900060320    5c                   enddo
023000060320    4c                   enddo
023100060320
023200060320      * controllo se AC PLUS presente
023300060321     c                   eval      kftc = 'C '
023400060320     c     ktpt          setll     titpt01l
023500060320    4c                   do        *hival
023600060320     c     ktpt          reade     titpt01l
023700060320     c                   if        %eof(titpt01l)
023800060320     c                   leave
023900060320     c                   endif
024000060320      * non annullate
024100060320     c                   if        tptatb <> *blanks
024200060320     c                   iter
024300060320     c                   endif
024400060323     c                   eval      wacplus = *on
024500060320      * AC PLUS reale/fittizia
024600060320     c                   eval      wtpttma = tpttma
024700060320      * AC PLUS par/arr/entrambe
024800060320     c                   eval      wtptapl = tptapl
024900060320      * controllo se AC PLUS reale e completa
025000060320     c                   if        tpttma = *blanks
025100060320     c                   select
025200060321     c                   when      tptapl = *blanks
025300060323     c                   eval      wacplusc = *on
025400060322     c                   when      tambap = 'P' and tptapl = 'P'
025500060323     c                   eval      wacplusc = *on
025600060322     c                   when      tambap = 'A' and tptapl = 'A'
025700060323     c                   eval      wacplusc = *on
025800060320     c                   endsl
025900060320     c                   leave
026000060321     c                   endif
026100060320    4c                   enddo
026200060320
026300060324      * controllo se AC PLUS 8 presente
026400060320     c                   eval      kftc = '8 '
026500060320     c     ktpt          setll     titpt01l
026600060320    4c                   do        *hival
026700060320     c     ktpt          reade     titpt01l
026800060320     c                   if        %eof(titpt01l)
026900060320     c                   leave
027000060320     c                   endif
027100060320      * non annullate
027200060320     c                   if        tptatb <> *blanks
027300060320     c                   iter
027400060320     c                   endif
027500060320     c                   eval      wacplusr = *on
027600060320     c                   leave
027700060320    4c                   enddo
027800060320
027900060324      * scrivo il file solo se:
028000060320     c                   select
028100060320      * sottoscrizione AC BASE
028200060323     c                   when      wacbase = *on
028300060320     c                   exsr      scrivi
028400060320      * rinuncia AC BASE
028500060323     c                   when      tamfmp = 'S'
028600060320     c                   exsr      scrivi
028700060320      * nessuna risposta
028800060323     c                   when      tamfmp = *blanks and
028900060323     c                             wacbase = *off and wacplusc = *off
029000060320     c                   exsr      scrivi
029100060323      * AC PLUS inserita
029200060323     c                   when      wacplus = *on
029300060323     c                   exsr      scrivi
029400060323     c                   endsl
029500060215
029600060320     c                   eval      wksc = tamksc
029700060320     c                   eval      wctr = tamctr
029800060215
029900060215      * se la tariffa che sto leggendo deve ancora andare in vigore la elaboro
030000060215      * poi però devo elaborare anche la tariffa precedente che è in vigore ora
030100060215     c                   if        tamddt > *date
030200060215     c                   eval      wctr = 999
030300060215     c                   endif
030400060320    3c                   endif
030500060320
030600060320    2c                   enddo
030700060215
030800060320    1c                   enddo
030900060320
031000060320      * stampo
031100060323     c                   if        parfile <> 'F'
031200060323     c   02              exsr      sr_stampa
031300060323     c  n02              exsr      sr_selez
031400060323     c  n02              write     ta58e1
031500060321     c                   endif
031600060215
031700060215     c                   eval      *inlr = *on
031800060215
031900060215      *------------------------------------------------------------------------*
032000060320      * SCRIVO IL FILE PER LA STAMPA
032100060215      *------------------------------------------------------------------------*
032200060215     c     scrivi        begsr
032300060215
032400060323     c  n02              eval      *in02 = *on
032500060323
032600060320     c                   clear                   wftac10
032700060320
032800060320     c                   movel     tamksc        w0030
032900060320     c     w0030         chain     azorg01l
033000060215
033100060320      * utente/data/ora elaborazione
033200060320     c                   eval      tacpru = knmus
033300060321     c                   eval      tacdte = wdata
033400060321     c                   eval      tacore = wora
033500060320      * distretto
033600060320     c                   eval      tacdis = orgfl3
033700060320     c                   eval      kcod = '17'
033800060320     c                   eval      kkey = tacdis
033900060320     c     ktabel        chain     tabel00f
034000060320     c                   if        not %found(tabel00f) or tblflg <> *blanks
034100060320     c                   eval      tacddis = *all'*'
034200060320     c                   else
034300060320     c                   eval      tacddis = tbluni
034400060320     c                   endif
034500060320      * area
034600060321     c                   eval      taccar = orgcar
034700060320     c                   eval      kcod = '05'
034800060321     c                   movel     taccar        kkey
034900060320     c     ktabel        chain     tabel00f
035000060320     c                   if        not %found(tabel00f) or tblflg <> *blanks
035100060321     c                   eval      tacdcar = *all'*'
035200060320     c                   else
035300060321     c                   eval      tacdcar = tbluni
035400060320     c                   endif
035500060320      * p.o.
035600060320     c                   eval      tacfil = orgfil
035700060320     c                   if        not %found(azorg01l) or orgfva <> *blanks
035800060321     c                   eval      tacdfil = *all'*'
035900060320     c                   else
036000060320     c                   eval      tacdfil = orgdes
036100060320     c                   endif
036200060320      * commerciale
036300060320     c                   eval      taccmm = clpage
036400130905     c     TACcmm        chain     AZCMM000
036500130905     c                   if        not %found(AZCMM01L)
036600060320     c                   eval      tacdcmm = *all'*'
036700060320     c                   else
036800130905     c                   eval      tacdcmm = CMMdes
036900060320     c                   endif
037000060215      * cliente
037100060320     c                   eval      tacksc = tamksc
037200060320     c                   eval      tacrag = acorag
037300060320      * indirizzo
037400060320     c                   eval      tacvia = indvia
037500060320     c                   eval      taccit = indcit
037600060322     c                   eval      taccap = indcae
037700060320     c                   eval      tacprv = indprv
037800060320     c                   eval      tacnaz = indsta
037900060321      * telefono
038000060321     c                   eval      tactel = indtel
038100060215      * partita IVA/codice fiscale
038200060215     c                   if        indiva <> *Blanks
038300060320     c                   eval      tacivacf = indiva
038400060215     c                   else
038500060215     c                   if        indcdf <> *Blanks
038600060320     c                   eval      tacivacf = indcdf
038700060215     c                   endif
038800060215     c                   endif
038900060320      * importanza cliente
039000060320     c                   eval      tacclv = clpclv
039100060215      * data ultima spedizione fattura
039200060320     c                   eval      tacdus = clpdus
039300060215      * tariffa
039400060320     c                   eval      tacctr = tamctr
039500060320     c                   eval      tacprg = tamprg
039600060320     c                   eval      tacddt = tamddt
039700060320     c                   eval      tacdst = tamdst
039800060323      * tariffa bloccata partenza/arrivo
039900060322     c                   if        tambap = *blanks
040000060322     c                   eval      tacbap = 'E'
040100060215     c                   else
040200060322     c                   eval      tacbap = tambap
040300060215     c                   endif
040400060320      * rifiuta AC BASE
040500060320     c                   if        tamfmp = 'S'
040600060320     c                   eval      tacfmp = '1'
040700060320     c                   else
040800060320     c                   eval      tacfmp = '0'
040900060320     c                   endif
041000060320      * presente AC BASE
041100060320     c                   if        wacbase = *on
041200060320     c                   eval      tacacb = '1'
041300060320     c                   else
041400060320     c                   eval      tacacb = '0'
041500060320     c                   endif
041600060323      * presente AC PLUS
041700060323     c                   if        wacplus = *on
041800060323      * completa
041900060323     c                   if        wacplusc = *on
042000060320     c                   eval      tacacp = '1'
042100060323     c                   else
042200060323     c                   eval      tacacp = '0'
042300060323     c                   endif
042400060321      * AC PLUS reale/fittizia
042500060321     c                   if        wtpttma = *blanks
042600060321     c                   eval      tactmaacp = 'R'
042700060321     c                   else
042800060321     c                   eval      tactmaacp = 'F'
042900060321     c                   endif
043000060321      * AC PLUS par/arr/entr.
043100060321     c                   if        wtptapl = *blanks
043200060321     c                   eval      tacaplacp = 'E'
043300060321     c                   else
043400060321     c                   eval      tacaplacp = wtptapl
043500060321     c                   endif
043600060324     c                   else
043700060324     c                   eval      tacacp = '0'
043800060320     c                   endif
043900060320      * nessuna risposta
044000060323     c                   if        wacbase = *off and wacplusc = *off and
044100060320     c                             tamfmp = *blanks
044200060323     c                   eval      tacnorisp = '1'
044300060320     c                   else
044400060323     c                   eval      tacnorisp = '0'
044500060320     c                   endif
044600060323      * presente AC PLUS 8
044700060320     c                   if        wacplusr = *on
044800060320     c                   eval      tacac8 = '1'
044900060320     c                   else
045000060320     c                   eval      tacac8 = '0'
045100060320     c                   endif
045200060726
045300060320      * discordanza AC BASE con CLV
045400060321     c                   if        wacbase = *on
045500060726      * per prima cosa controllo se il cliente è presente nella tabella della
045600060726      * forzatura AC BASE in modo da fare il confronto con quanto inserito
045700060726      * in tabella
045800060726     c                   clear                   tibs02ds
045900060726     c                   clear                   dfab
046000060726     c                   eval      t02mod = 'C'
046100060726     c                   eval      t02sif = knsif
046200060726     c                   eval      t02cod = 'FAB'
046300060726     c                   movel     tamksc        t02ke1
046400060726     c                   call      'TIBS02R'
046500060726     c                   parm                    kpjba
046600060726     c                   parm                    tibs02ds
046700060726      * se lo trovo faccio il confronto e non faccio il confronto con CLV
046800060726     c                   if        t02err = *blanks
046900060726     c                   eval      dfab = t02uni
047000060726     c                   if        d§fabitr > 0 and witrd < d§fabitr
047100060726     c                   eval      tacdisc = '1'
047200060726     c                   else
047300060726     c                   eval      tacdisc = '0'
047400060726     c                   endif
047500060726     c                   goto      noclv
047600060726     c                   endif
047700060726      * controllo con CLV
047800060320     c                   select
047900060320      * clienti Top o Direzionali
048000060320     c                   when      (clpclv = 'T' or clpclv = 'D') and
048100111130     c                             witrd < 0,003
048200111130     c***                          witrd < 0,002
048300060320     c                   eval      tacdisc = '1'
048400060320      * clienti A
048500111130     c***                when      clpclv = 'A' and witrd < 0,003
048600111130     c                   when      clpclv = 'A' and witrd < 0,004
048700060320     c                   eval      tacdisc = '1'
048800060320      * clienti C o B
048900060320     c                   when      (clpclv = 'C' or clpclv = 'B') and
049000111130     c                             witrd < 0,007
049100111130     c***                          witrd < 0,005
049200060320     c                   eval      tacdisc = '1'
049300060320     c                   other
049400060320     c                   eval      tacdisc = '0'
049500060320     c                   endsl
049600060321     c                   endif
049700060726     c     noclv         tag
049800060726
049900060320      * importo AC BASE
050000060320     c                   eval      tacitracb = witrd
050100060215      * scrivo file
050200060320     c                   write     wftac10
050300060320
050400060320      * se richiesto genero il file di work
050500060321     c                   if        parfile <> 'S'
050600060320     c                   clear                   wftac000
050700060320     c                   eval      dswftac0 = dswftac1
050800060320     c                   write     wftac000
050900060320     c                   endif
051000060215
051100060215     c                   endsr
051200060320
051300060320      *--------------------------------------------------------------*
051400060320      * STAMPO
051500060320      *--------------------------------------------------------------*
051600060320     c     sr_stampa     begsr
051700060321
051800060321      * stampo la pagina con le selezioni
051900060321     c                   exsr      sr_selez
052000060320
052100060320      * leggo file di comodo per la stampa
052200060323     c  n01*loval        setll     wftac11l
052300060323     c   01*loval        setll     wftac12l
052400060321     c                   do        *hival
052500060323     c  n01              read      wftac11l
052600060323     c   01              read      wftac12l
052700060322     c                   if        %eof
052800060320     c                   leave
052900060321     c                   endif
053000060324      * controllo se è da stampare in base alle selezioni fatte
053100060324     c                   exsr      sr_ctrrec
053200060324     c                   if        wok = *on
053300060324     c                   iter
053400060324     c                   endif
053500060321
053600060320      * rottura per distretto
053700060321     c                   if        pdis <> tacdis
053800060321      * totali per distretto
053900060322     c                   if        pdis <> *blanks and *in12
054000060324      * prima stampo il totale del p.o.
054100060322     c                   write     ta58tf1
054200060322     c                   clear                   ptfksc
054300060322     c                   clear                   ptfctr
054400060322     c                   clear                   ptffmp
054500060322     c                   clear                   ptfacb
054600060322     c                   clear                   ptfacp
054700060322     c                   clear                   ptfrisp
054800060322     c                   clear                   ptfdisc
054900060324      * salto pagina
055000060324     c                   write     ta58t1
055100060324     c                   write     ta58t2
055200060324     c                   write     ta58t3
055300060324      * poi stampo il totale area
055400060322     c                   write     ta58tc1
055500060322     c                   clear                   ptcksc
055600060322     c                   clear                   ptcctr
055700060322     c                   clear                   ptcfmp
055800060322     c                   clear                   ptcacb
055900060322     c                   clear                   ptcacp
056000060322     c                   clear                   ptcrisp
056100060322     c                   clear                   ptcdisc
056200060324      * salto pagina
056300060324     c                   write     ta58t1
056400060324     c                   write     ta58t2
056500060324     c                   write     ta58t3
056600060324      * infine stampo il totale del distretto
056700060321     c                   write     ta58td1
056800060321     c                   clear                   ptdksc
056900060321     c                   clear                   ptdctr
057000060321     c                   clear                   ptdfmp
057100060321     c                   clear                   ptdacb
057200060321     c                   clear                   ptdacp
057300060321     c                   clear                   ptdrisp
057400060321     c                   clear                   ptddisc
057500060321     c                   endif
057600060324      * stampo la testata
057700060321     c                   eval      pdis = tacdis
057800060321     c                   eval      pdesdis = tacddis
057900060321     c                   eval      pcar = taccar
058000060321     c                   eval      pdescar = tacdcar
058100060321     c                   eval      pfil = tacfil
058200060321     c                   eval      pdesfil = tacdfil
058300060321     c                   eval      pcmm = taccmm
058400060321     c                   eval      pdescmm = tacdcmm
058500060321     c                   write     ta58t1
058600060321     c                   write     ta58t2
058700060321     c                   write     ta58t3
058800060321     c                   write     ta58t4
058900060321     c                   endif
059000060320      * rottura per area
059100060322     c                   if        pcar <> taccar
059200060321      * totali per area
059300060323     c                   if        pcar <> *zeros and (*in46 or *in12)
059400060324      * prima stampo il totale del p.o.
059500060322     c                   write     ta58tf1
059600060322     c                   clear                   ptfksc
059700060322     c                   clear                   ptfctr
059800060322     c                   clear                   ptffmp
059900060322     c                   clear                   ptfacb
060000060322     c                   clear                   ptfacp
060100060322     c                   clear                   ptfrisp
060200060322     c                   clear                   ptfdisc
060300060324      * salto pagina
060400060324     c                   write     ta58t1
060500060324     c                   write     ta58t2
060600060324     c                   write     ta58t3
060700060324      * poi stampo il totale area
060800060321     c                   write     ta58tc1
060900060321     c                   clear                   ptcksc
061000060321     c                   clear                   ptcctr
061100060321     c                   clear                   ptcfmp
061200060321     c                   clear                   ptcacb
061300060321     c                   clear                   ptcacp
061400060321     c                   clear                   ptcrisp
061500060321     c                   clear                   ptcdisc
061600060321     c                   endif
061700060324      * stampo la testata
061800060321     c                   eval      pdis = tacdis
061900060321     c                   eval      pdesdis = tacddis
062000060321     c                   eval      pcar = taccar
062100060321     c                   eval      pdescar = tacdcar
062200060321     c                   eval      pfil = tacfil
062300060321     c                   eval      pdesfil = tacdfil
062400060321     c                   eval      pcmm = taccmm
062500060321     c                   eval      pdescmm = tacdcmm
062600060321     c                   write     ta58t1
062700060321     c                   write     ta58t2
062800060321     c                   write     ta58t3
062900060321     c                   write     ta58t4
063000060321     c                   endif
063100060320      * rottura per p.o.
063200060321     c                   if        pfil <> tacfil
063300060324      * totali per p.o.
063400060321     c                   if        pfil <> *zeros
063500060321     c                   write     ta58tf1
063600060321     c                   clear                   ptfksc
063700060321     c                   clear                   ptfctr
063800060321     c                   clear                   ptffmp
063900060321     c                   clear                   ptfacb
064000060321     c                   clear                   ptfacp
064100060321     c                   clear                   ptfrisp
064200060321     c                   clear                   ptfdisc
064300060321     c                   endif
064400060321      * testata
064500060321     c                   eval      pdis = tacdis
064600060321     c                   eval      pdesdis = tacddis
064700060321     c                   eval      pcar = taccar
064800060321     c                   eval      pdescar = tacdcar
064900060321     c                   eval      pfil = tacfil
065000060321     c                   eval      pdesfil = tacdfil
065100060321     c                   eval      pcmm = taccmm
065200060321     c                   eval      pdescmm = tacdcmm
065300060321     c                   write     ta58t1
065400060321     c                   write     ta58t2
065500060321     c                   write     ta58t3
065600060321     c                   write     ta58t4
065700060320     c                   endif
065800060321
065900060321      * rottura per commerciale
066000060321     c                   if        pcmm <> taccmm
066100060321     c                   eval      pdis = tacdis
066200060321     c                   eval      pdesdis = tacddis
066300060321     c                   eval      pcar = taccar
066400060321     c                   eval      pdescar = tacdcar
066500060321     c                   eval      pfil = tacfil
066600060321     c                   eval      pdesfil = tacdfil
066700060321     c                   eval      pcmm = taccmm
066800060321     c                   eval      pdescmm = tacdcmm
066900060321      * salto pagina
067000060321     c                   if        *in90 = *on
067100060321     c                   write     ta58t1
067200060321     c                   write     ta58t2
067300060321     c                   write     ta58t3
067400060321     c                   write     ta58t4
067500060321     c                   eval      *in90 = *off
067600060321     c                   else
067700060322     c                   write     ta58t4
067800060321     c                   endif
067900060321     c                   endif
068000060321
068100060321      * conto quanti clienti
068200060321     c                   if        savksc <> tacksc
068300060321     c                   add       1             tksc
068400060321     c                   add       1             ptdksc
068500060321     c                   add       1             ptcksc
068600060321     c                   add       1             ptfksc
068700060321     c                   eval      savksc = tacksc
068800060321     c                   endif
068900060321      * conto quante tariffe
069000060321     c                   add       1             tctr
069100060321     c                   add       1             ptdctr
069200060321     c                   add       1             ptcctr
069300060321     c                   add       1             ptfctr
069400060321
069500060321      * ragione sociale
069600060321     c                   eval      prag = tacrag
069700060321      * validità tariffa
069800060322     c                   clear                   pbap
069900060321     c                   select
070000060322     c                   when      tacbap = 'P'
070100060322     c                   eval      pbap = 'PAR'
070200060322     c                   when      tacbap = 'A'
070300060322     c                   eval      pbap = 'ARR'
070400060322     c                   when      tacbap = 'E'
070500060323     c                   eval      pbap = 'P+A'
070600060321     c                   endsl
070700060321      * rifiuta AC BASE
070800060321     c                   clear                   pfmp
070900060321     c                   if        tacfmp = '0'
071000060321     c                   eval      pfmp = 'NO'
071100060321     c                   endif
071200060321     c                   if        tacfmp = '1'
071300060321     c                   eval      pfmp = 'SI'
071400060321     c                   add       1             tfmp
071500060321     c                   add       1             ptdfmp
071600060321     c                   add       1             ptcfmp
071700060321     c                   add       1             ptffmp
071800060321     c                   endif
071900060321      * presente AC BASE
072000060321     c                   clear                   pacb
072100060321     c                   if        tacacb = '0'
072200060321     c                   eval      pacb = 'NO'
072300060321     c                   endif
072400060321     c                   if        tacacb = '1'
072500060321     c                   eval      pacb = 'SI'
072600060321     c                   add       1             tacb
072700060321     c                   add       1             ptdacb
072800060321     c                   add       1             ptcacb
072900060321     c                   add       1             ptfacb
073000060321     c                   endif
073100060324      * presente AC PLUS completa
073200060321     c                   clear                   pacp
073300060321     c                   if        tacacp = '0'
073400060321     c                   eval      pacp = 'NO'
073500060321     c                   endif
073600060321     c                   if        tacacp = '1'
073700060321     c                   eval      pacp = 'SI'
073800060321     c                   add       1             tacp
073900060321     c                   add       1             ptdacp
074000060321     c                   add       1             ptcacp
074100060321     c                   add       1             ptfacp
074200060321     c                   endif
074300060321      * validità AC PLUS
074400060321     c                   clear                   papl
074500060324     c                   if        tactmaacp = 'R'
074600060321     c                   select
074700060321     c                   when      tacaplacp = 'P'
074800060321     c                   eval      papl = 'PAR'
074900060321     c                   when      tacaplacp = 'A'
075000060321     c                   eval      papl = 'ARR'
075100060321     c                   when      tacaplacp = 'E'
075200060323     c                   eval      papl = 'P+A'
075300060321     c                   endsl
075400060324     c                   endif
075500060321      * risposta
075600060321     c                   clear                   prisp
075700060323     c                   if        tacnorisp = '1'
075800060321     c                   eval      prisp = 'NO'
075900060321     c                   add       1             trisp
076000060321     c                   add       1             ptdrisp
076100060321     c                   add       1             ptcrisp
076200060321     c                   add       1             ptfrisp
076300060321     c                   endif
076400060323     c                   if        tacnorisp = '0'
076500060321     c                   eval      prisp = 'SI'
076600060321     c                   endif
076700060321      * discordanza
076800060321     c                   clear                   pdisc
076900060321     c                   if        tacdisc = '0'
077000060321     c                   eval      pdisc = 'NO'
077100060321     c                   endif
077200060321     c                   if        tacdisc = '1'
077300060321     c                   eval      pdisc = 'SI'
077400060321     c                   add       1             tdisc
077500060321     c                   add       1             ptddisc
077600060321     c                   add       1             ptcdisc
077700060321     c                   add       1             ptfdisc
077800060321     c                   endif
077900060321      * importo AC BASE
078000060321     c                   eval      pitr = tacitracb
078100060321      * salto pagina
078200060324     c                   if        *in90 = *on
078300060321     c                   write     ta58t1
078400060321     c                   write     ta58t2
078500060321     c                   write     ta58t3
078600060321     c                   write     ta58t4
078700060321     c                   eval      *in90 = *off
078800060321     c                   endif
078900060321      * stampo dettaglio
079000060321     c                   write     ta58d1
079100060320
079200060320 e1  c                   enddo
079300060321
079400060321      * totale generale
079500060324      * prima stampo il totale del p.o.
079600060322     c                   write     ta58tf1
079700060324      * salto pagina
079800060324     c                   if        *in12 or *in46
079900060324     c                   write     ta58t1
080000060324     c                   write     ta58t2
080100060324     c                   write     ta58t3
080200060324      * stampo il totale dell'area
080300060324     c                   write     ta58tc1
080400060324     c                   endif
080500060324      * salto pagina
080600060324     c                   if        *in12
080700060324     c                   write     ta58t1
080800060324     c                   write     ta58t2
080900060324     c                   write     ta58t3
081000060324      * stampo il totale del distretto
081100060324     c                   write     ta58td1
081200060324      * salto pagina
081300060324     c                   write     ta58t1
081400060324     c                   write     ta58t2
081500060324     c                   write     ta58t3
081600060324      * stampo il totale generale
081700060324     c                   write     ta58tt
081800060324     c                   endif
081900060320
082000060320      * fine stampa
082100060321     c                   write     ta58e1
082200060320
082300060320     c                   endsr
082400060321
082500060321      *--------------------------------------------------------------*
082600060321      * STAMPO LA PAGINA DELLE SELEZIONI
082700060321      *--------------------------------------------------------------*
082800060321     c     sr_selez      begsr
082900060321
083000060321     c                   if        parfil = *zeros
083100060321     c                   eval      desfil = 'Tutti'
083200060321     c                   else
083300060321     c     parfil        chain     azorg01l
083400060321     c                   if        %found(azorg01l)
083500060321     c                   eval      desfil = orgdes
083600060321     c                   endif
083700060321     c                   endif
083800060321
083900060321     c                   eval      parcar01 = parcar(01)
084000060321     c                   eval      parcar02 = parcar(02)
084100060321     c                   eval      parcar03 = parcar(03)
084200060321     c                   eval      parcar04 = parcar(04)
084300060321     c                   eval      parcar05 = parcar(05)
084400060321     c                   eval      parcar06 = parcar(06)
084500060321     c                   eval      parcar07 = parcar(07)
084600060321     c                   eval      parcar08 = parcar(08)
084700060321     c                   eval      parcar09 = parcar(09)
084800060321     c                   eval      parcar10 = parcar(10)
084900060321
085000060321     c                   write     ta58s1
085100060321
085200060321     c                   endsr
085300060324
085400060324      *--------------------------------------------------------------*
085500060324      * CONTROLLO SE IL RECORD È DA STAMPARE
085600060324      *--------------------------------------------------------------*
085700060324     c     sr_ctrrec     begsr
085800060324
085900060324     c                   eval      wok = *on
086000060324
086100060324     c                   select
086200060324      * sottoscrizione AC BASE
086300060324     c                   when      parsc01 = 'X' and tacacb = '1'
086400060324     c                   eval      wok = *off
086500060324      * rinuncia AC BASE
086600060324     c                   when      parsc02 = 'X' and tacfmp = '1'
086700060324     c                   eval      wok = *off
086800060324      * nessuna risposta
086900060324     c                   when      parsc03 = 'X' and tacnorisp = '1'
087000060324     c                   eval      wok = *off
087100060324      * discordanza
087200060324     c                   when      parsc04 = 'X' and tacdisc = '1'
087300060324     c                   eval      wok = *off
087400060324     c                   endsl
087500060324
087600060324     c                   endsr
087700060320
087800060320      *--------------------------------------------------------------*
087900060320      * ROUTINE INIZIALE
088000060320      *--------------------------------------------------------------*
088100060320     c     *inzsr        begsr
088200060320
088300060320     c     *entry        plist
088400060320     c                   parm                    kpjba
088500060320     c                   movel     kpjbu         parm01
088600060320
088700060320     c     *dtaara       define    §azute        azuteds
088800060320     c     *dtaara       define    §datiute      ddatiute
088900060320
089000060320     c                   in(e)     *dtaara
089100060320     c                   if        %error or rsut = *blanks
089200060320     c                   clear                   tibs34ds
089300060320     c                   call      'TIBS34R'
089400060320     c                   parm                    tibs34ds
089500060320     c                   in        *dtaara
089600060320     c                   endif
089700060320
089800060320
089900060320      * se s.i. di prova imposto la libreria di prova
090000060320     c                   if        %subst(knsif:7:1) = 'P'
090100060320     c                   eval      wlib = 'GAITRAAZP '
090200060320     c                   else
090300060320     c                   eval      wlib = 'GAITRAAZM '
090400060320     c                   endif
090500060320
090600060320      * se richiesta la generazione del file lo pulisco
090700060321     c                   if        parfile <> 'S'
090800060320     c                   clear                   comman
090900060320     c                   movel(p)  cmd(1)        comman
091000060320     c                   eval      %subst(comman:13:9) = wlib
091100060320     c                   call      'QCMDEXC'
091200060320     c                   parm                    comman
091300060320     c                   parm                    lenght
091400060320     c                   endif
091500060320
091600060320      * duplico il file di comodo in qtemp
091700060320     c                   clear                   comman
091800060320     c                   movel(p)  cmd(2)        comman
091900060320     c                   eval      %subst(comman:32:9) = wlib
092000060320     c                   call      'QCMDEXC'
092100060320     c                   parm                    comman
092200060320     c                   parm                    lenght
092300060320
092400060320      * apro il file di comodo per la stampa
092500060320     c                   open      wftac10f
092600060322      * se ordinata per codice apro la vista logica x ksc
092700060322     c                   if        parord = 'C'
092800060322     c                   open      wftac11l
092900060323     c                   eval      *in01 = *off
093000060322     c                   else
093100060322      * se ordinata per ragione sociale apro vista logica x rag
093200060322     c                   open      wftac12l
093300060323     c                   eval      *in01 = *on
093400060322     c                   endif
093500060320
093600060320      * se richiesta la creazione del file lo apro
093700060321     c                   if        parfile <> 'S'
093800060320     c                   open      wftac00f
093900060320     c                   endif
094000060320
094100060320      * reperisco data e ora del lancio
094200060320     c                   time                    w0140
094300060320     c                   move      w0140         wdata
094400060320     c                   movel     w0140         wora
094500060320
094600060320      * carico in sk i p.o. PPT - DPD - EEX
094700060320     c                   clear                   sknocl
094800060320     c     *loval        setll     azorg01l
094900060320     c                   do        *hival
095000060320     c                   read      azorg01l
095100060320      * fine file
095200060320     c                   if        %eof(azorg01l)
095300060320     c                   leave
095400060320     c                   endif
095500060320      * escludo i p.o. non validi
095600060320     c                   if        orgfva <> *blanks or
095700060320     c                             (orgfag <> 'A' and orgfag <> 'F')
095800060320     c                   iter
095900060320     c                   endif
096000060320     c                   eval      og143 = orgde3
096100060323      * escludo i p.o. non PPT - DPD - EEX - FED
096200060321     c                   if        §ogntw <> 'PPT' and §ogntw <> 'DPD' and
096300060323     c                             §ogntw <> 'EEX' and §ogntw <> 'FED'
096400060320     c                   iter
096500060320     c                   endif
096600060320     c     orgfil        lookup    sknocl                                 30
096700060320     c                   if        not *in30
096800060320     c                   eval      yy = 1
096900060320     c     *zeros        lookup    sknocl(yy)                             30
097000060320     c                   if        *in30
097100060320     c                   eval      sknocl(yy) = orgfil
097200060320     c                   endif
097300060320     c                   endif
097400060320     c                   enddo
097500060320
097600060320      * pulisco la sk dei p.o. da elaborare
097700060320     c                   clear                   skpoel
097800060320
097900060320      * se ho le aree carico una sk con tutti i p.o. relativi alle aree
098000060320      * richieste
098100060321    1c                   if        parcar(01) <> *blanks
098200060320    2c                   do        10            xx
098300060321    3c                   if        parcar(xx) <> *blanks
098400060321     c                   move      parcar(xx)    kcar
098500060320     c     kazorg        setll     azorg02l
098600060320    4c                   do        *hival
098700060320     c     kazorg        reade     azorg02l
098800060320     c                   if        %eof(azorg02l)
098900060320     c                   leave
099000060320     c                   endif
099100060320     c                   if        orgfva <> *blanks or
099200060320     c                             (orgfag <> 'A' and orgfag <> 'F')
099300060320     c                   iter
099400060320     c                   endif
099500060320     c                   move      orgfil        w003a
099600060320     c     w003a         lookup    skpoel                                 30
099700060320     c                   if        not *in30
099800060320     c                   eval      yy = 1
099900060320     c     *blanks       lookup    skpoel(yy)                             30
100000060320     c                   if        *in30
100100060320     c                   eval      skpoel(yy) = w003a
100200060320     c                   endif
100300060320     c                   endif
100400060320    4c                   enddo
100500060320    3c                   endif
100600060320    2c                   enddo
100700060320
100800060320   x1c                   else
100900060320      * carico il p.o. richiesto a video
101000060321    2c                   if        parfil <>  *zeros
101100060321     c                   move      parfil        skpoel(1)
101200060320   x2c                   else
101300060320     c                   clear                   trul31ds
101400060321     c                   eval      i31abi = parabi
101500060320     c                   eval      i31cdi = dutdis
101600060320     c                   eval      i31car = dutare
101700060320     c                   eval      i31cpo = dutpou
101800060320     c                   call      'TRUL31R'
101900060320     c                   parm                    kpjba
102000060320     c                   parm                    trul31ds
102100060320     c                   if        o31pog > *zeros
102200060320     c                   movea     o31pog        skpoel
102300060320     c                   endif
102400060320     c                   endif
102500060320     c                   endif
102600060321
102700060321      * abilitazione distretto
102800060322     c                   if        parabi = 'DI' and parcar(01) <> *blanks
102900060321     c                   eval      *in46 = *on
103000060321     c                   endif
103100060321
103200060321     c                   eval      *in12 = (simfel = *zeros)
103300060320
103400060320     c     kazorg        klist
103500060320     c                   kfld                    dutdis
103600060320     c                   kfld                    kcar
103700060320
103800060320     c     ktpt          klist
103900060320     c                   kfld                    tamksc
104000060320     c                   kfld                    tamctr
104100060320     c                   kfld                    tamprg
104200060320     c                   kfld                    kftc
104300060320
104400060320     c     ktabel        klist
104500060320     c                   kfld                    codut
104600060320     c                   kfld                    kcod
104700060320     c                   kfld                    kkey
104800060320
104900060320     c                   endsr
105000060320      *--------------------------------------------------------------*
105100060320**   cmd - comando
105200060320CLRPFM FILE(         /WFTAC00F)
105300060320CRTDUPOBJ OBJ(WFTAC1*) FROMLIB(         ) OBJTYPE(*FILE) TOLIB(QTEMP)
