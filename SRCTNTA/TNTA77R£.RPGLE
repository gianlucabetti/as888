000100060320     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200060320      *------------------------------------------------------------------------*
000300060320      *
000400070123      *  Stato avanzamento Istituto FUEL
000500060320      *
000600060320      *------------------------------------------------------------------------*
000700060215
000800060215     ftntam04l  if   e           k disk
000900060320     ftitpd01l  if   e           k disk
001000110224     fTITPT01L  if   e           k disk
001100110224     fTIPMG01L  if   e           k disk
001200130905     fAZCMM01L  if   e           k disk
001300060320     fazorg01l  if   e           k disk
001400060320     ftabel00f  if   e           k disk
001500080925     fwftfu10f  o    e             disk    usropn
001600130403     f                                     extfile(wLibWF)
001700070124     ftnta77p   o    e             printer oflind(*in90)
001800060323
001900060323      *------------------------------------------------------------------------*
002000060323      *  RIEPILOGO INDICATORI
002100060323      *------------------------------------------------------------------------*
002200060323      * 02 - ho scritto almeno 1 rcd
002300060323      * 12 - utente di sede
002400060323      * 30 - comodo
002500060215
002600060215      *------------------------------------------------------------------------*
002700060215      *   V A R I A B I L I
002800060215      *------------------------------------------------------------------------*
002900060320     d comman          s             80
003000060320     d codut           s              1  0 inz(1)
003100060320     d kcod            s                   like(tblcod)
003200060320     d kksc            s                   like(tamksc)
003300060320     d kkey            s                   like(tblkey)
003400060320     d lenght          s             15  5 inz(80)
003500070124     d kftc            s                   like(tpdftc)
003600060215     d wctr            s                   like(tamctr)
003700060320     d wdata           s              8  0
003800080924     d wfd1            s                   like(tpditr)
003900080924     d wfd2            s                   like(tpditr)
004000080924     d wfd3            s                   like(tpditr)
004100080924     d wsg1            s                   like(tpdsgl)
004200080924     d wsg2            s                   like(tpdsgl)
004300080924     d wsg3            s                   like(tpdsgl)
004400080924     d wcts            s                   like(tpdcts)
004500060215     d wksc            s                   like(tamksc)
004600060320     d wlib            s              9
004700060320     d wora            s              6  0
004800060320     d w0030           s              3  0
004900060320     d w0140           s             14  0
005000070126     d contatore       s              2  0
005100080924     d overcts         s              1
005200130403
005300130403       // -?Nomi estesi libreria/file dei file?
005400130403     d wLibWF          s             21a   inz
005500060320
005600060320      *------------------------------------------------------------------------*
005700060320      *   S C H I E R E
005800060320      *------------------------------------------------------------------------*
005900070123     d cmd             s             80    dim(1) ctdata perrcd(1)
006000060215
006100060215      *------------------------------------------------------------------------*
006200060215      *   D S   I N T E R N E / E S T E R N E
006300060215      *------------------------------------------------------------------------*
006400080922     d tnta86ds        ds                  inz
006500080922     d  dsdip                         6  0
006600080922     d  dsdfp                         6  0
006700081008     d  dsdru                         8  0
006800130403     d  dsDIV                         8  0
006900130403     d  ds10F                         1
007000060321
007100060321     d wlbdat          ds                  inz
007200060321     d  g02dat                 1      8  0
007300060321     d  g02inv                 9     16  0
007400060321     d  g02err                17     17
007500060321     d  g02tgi                18     22  0
007600060320
007700060320     d azuteds       e ds                  extname(azute00f)
007800060320     d ddatiute      e ds
007900060320     d ds_cnaco      e ds                  inz  extname(Cnaco00f)
008000060320     d ds_cnind      e ds                  inz  extname(Cnind00f)
008100060320     d ds_cnclp      e ds                  inz  extname(Cnclp00f)
008200060320     d ds_fncls      e ds                  inz  extname(Fncls00f)
008300060215     d kpjba         e ds
008400060215     d dsta01        e ds
008500070124     d tibs10ds      e ds
008600060320     d tibs34ds      e ds
008700060320     d tibs69ds      e ds
008800080922     d tise70ds      e ds
008900140116
009000140116       // -?DS campo TPTflo
009100140116     d dTPT01        e ds
009200060215
009300060215      *------------------------------------------------------------------------*
009400060320
009500060320     c                   clear                   wksc
009600060215     c                   eval      wctr = 999
009700060320     c                   move      *all'0'       kksc
009800060215
009900060320     c     kksc          setll     tntam04l
010000070124    1c                   do        *hival
010100060215     c                   read      tntam04l
010200060215      * fine file
010300060215     c                   if        %eof(tntam04l)
010400060215     c                   leave
010500060215     c                   endif
010600130403
010700130403      * Verifico data decorrenza e scadenza della tariffa con i dati passati
010800130403
010900130403      * se data decorrenza e scadenza della tariffa è maggiore della data
011000130403      * validità della tariffa leggo la successiva
011100130403     c                   if        tamddt > dsdiv and tamdst > dsdiv
011200130403     c                   iter
011300130403     c                   endif
011400060320
011500060320      * cambio tariffa
011600070124    2c                   if        tamctr <> wctr or tamksc <> wksc
011700060320
011800060320     c                   movel     tamflo        dsta01
011900060320
012000060320      * escludo le annullate
012100060215     c                   if        tamatb <> *blanks
012200060215     c                   iter
012300060215     c                   endif
012400060320      * escludo le bloccate
012500111221     c**!!!              if        tambap = 'B'
012600111221     c**!!!              iter
012700111221     c**!!!              endif
012800070123      * escludo le tariffe  FED
012900070123     c                   if        §tafed = 'S'
013000060320     c                   iter
013100060320     c                   endif
013200060320
013300060320      * cerco i dati anagrafici
013400060320     c                   clear                   tibs69ds
013500060320     c                   move      tamksc        i69kac
013600060320     c                   move      tamksc        i69kcp
013700060320     c                   call      'TIBS69R'
013800060320     c                   parm                    tibs69ds
013900060320     c                   parm                    ds_cnaco
014000060320     c                   parm                    ds_cnind
014100060320     c                   parm                    ds_cnclp
014200060320     c                   parm                    ds_fncls
014300060320      * escludo le tariffe dei clienti bloccati
014400130315     c                   if        acoabl <> *blanks
014500060320     c                   iter
014600060320     c                   endif
014700111221
014800111221      * solo se non bloccata
014900111221     c                   if        tambap <> 'B'
015000060320
015100070123      * controllo se Fuel presente
015200070123     c                   eval      kftc = 'f '
015300080924     c                   clear                   wfd1
015400080924     c                   clear                   wfd2
015500080924     c                   clear                   wfd3
015600080924     c                   clear                   wsg1
015700080924     c                   clear                   wsg2
015800080924     c                   clear                   wsg3
015900080924     c                   clear                   wcts
016000080924     c                   clear                   overcts
016100070126     c                   eval      contatore = 0
016200070123      * cerco importo percentuale
016300060320     c     ktpt          setll     titpd01l
016400070124    3c                   do        *hival
016500060320     c     ktpt          reade     titpd01l
016600060320      * fine file
016700060320     c                   if        %eof(titpd01l)
016800060320     c                   leave
016900060320     c                   endif
017000060320      * non annullate
017100060320     c                   if        tpdatb <> *blanks
017200060320     c                   iter
017300060320     c                   endif
017400080924      * se il codice tassazione è diverso da blank verifico se è lo stesso
017500080924      * altrimenti valorizzo il flag di + codici tassazione
017600080924     c                   If        wcts = *blank
017700080924     c                   eval      wcts = tpdcts
017800080924     c                   endif
017900080924
018000080924     c                   if        wcts <> tpdcts
018100080924     c                   eval      overcts = 'S'
018200080924     c                   leave
018300080924     c                   endif
018400080924      * scaglioni
018500070126     c                   add       1             contatore
018600080924      * uniformo lo scaglione 99999 tutti quelli che lo scaglione maggiore 99
018700080924     c                   if        tpdsgl >= 99
018800080924     c                   eval      tpdsgl = 99999,000
018900080924     c                   endif
019000080924
019100070126     c                   if        contatore = 1
019200080924     c                   eval      wfd1 = tpditr
019300080924     c                   eval      wsg1 = tpdsgl
019400080924     c                   iter
019500080924     c                   endif
019600080924
019700080924     c                   if        contatore = 2
019800080924     c                   eval      wfd2 = tpditr
019900080924     c                   eval      wsg2 = tpdsgl
020000080924     c                   iter
020100080924     c                   endif
020200080924
020300080924     c                   if        contatore = 3
020400080924     c                   eval      wfd3 = tpditr
020500080924     c                   eval      wsg3 = tpdsgl
020600080924     c                   iter
020700080924     c                   endif
020800080924
020900080924     c                   if        contatore > 3
021000060320     c                   leave
021100070124     c                   endif
021200080924
021300070124    3c                   enddo
021400070123      * scrivo il file
021500060320     c                   exsr      scrivi
021600111221     c                   endif
021700060215
021800060320     c                   eval      wksc = tamksc
021900060320     c                   eval      wctr = tamctr
022000060215
022100060215      * se la tariffa che sto leggendo deve ancora andare in vigore la elaboro
022200070124      * ma devo elaborare anche la tariffa precedente che è in vigore ora
022300130403      * => In realtà ora considero la data (ricevuta come parametro) in cui
022400130403      *    entra in vigore la tariffa - non più la data odierna
022500130403     c**!!!              if        tamddt > *date
022600130403     c**!!!              eval      wctr = 999
022700130403     c**!!!              endif
022800070124    2c                   endif
022900060215
023000060320    1c                   enddo
023100060320
023200070124      * stampo  la fine elaborazione
023300070124     c                   write     ta77s1
023400060215
023500060215     c                   eval      *inlr = *on
023600060215
023700060215      *------------------------------------------------------------------------*
023800060320      * SCRIVO IL FILE PER LA STAMPA
023900060215      *------------------------------------------------------------------------*
024000060215     c     scrivi        begsr
024100060215
024200060323     c  n02              eval      *in02 = *on
024300060323
024400070123     c                   clear                   wftfu000
024500060320
024600060320      * utente/data/ora elaborazione
024700070123     c                   eval      tfupru = knmus
024800070123     c                   eval      tfudte = wdata
024900070123     c                   eval      tfuore = wora
025000060215      * cliente
025100070123     c                   eval      tfuksc  = tamksc
025200070123     c                   eval      tfukscd = acorag
025300060215      * data ultima spedizione fattura
025400080923     c                   if        clpdus > *zeros
025500080923      * controllo la data lunga 8
025600080923     c                   clear                   wlbdat
025700080923     c                   z-add     clpdus        g02inv
025800080923     c                   eval      g02err = '3'
025900080923     c                   call      'XSRDA8'
026000080923     c                   parm                    wlbdat
026100080923     c                   eval      tfudus = g02inv
026200080923     c                   endif
026300060215      * tariffa
026400070123     c                   eval      tfuctr = tamctr
026500070123     c                   eval      tfuprg = tamprg
026600070123     c                   eval      tfuddt = tamddt
026700070123     c                   eval      tfudst = tamdst
026800070904      * tipo tariffa
026900070904     c                   select
027000070904     c                   when      §tadpd = 'S'
027100070904     c                   eval      tfutpt = 'D'
027200070904     c                   when      §tafed = 'S'
027300070904     c                   eval      tfutpt = 'F'
027400070904     c                   other
027500070904     c                   eval      tfutpt = tamfie
027600070904     c                   endsl
027700080924      * Fattore demoltiplicatore per ogni scaglione
027800080924     c                   eval      tfufd1 = wfd1
027900080924     c                   eval      tfusg1 = wsg1
028000080924     c                   eval      tfufd2 = wfd2
028100080924     c                   eval      tfusg2 = wsg2
028200080924     c                   eval      tfufd3 = wfd3
028300080924     c                   eval      tfusg3 = wsg3
028400080924      * se ci sono più codici tassazione valorizzo il flag
028500080924     c                   if        overcts = 'S'
028600080924     c                   eval      tfufct = 'S'
028700080924     c                   endif
028800080924      * se ci sono più scaglioni per lo stesso codice tassazione
028900080924     c                   if        contatore > 3
029000080924     c                   eval      tfufsg = 'S'
029100080924     c                   endif
029200070124      * cerco il codice cliente unificante
029300070124     c                   clear                   tibs10ds
029400070124     c                   eval      d10tle = 'ST'
029500070124     c                   eval      d10paf = 'P'
029600070124     c                   z-add     tfuksc        d10cod
029700081008     c                   z-add     dsdru         d10drf
029800070124     c                   CALL      'TIBS10R'
029900070124     c                   parm                    tibs10ds
030000070124     c                   parm      'A'           partpexe          1
030100070124      * se ci sono errori valorizzo come unificante il cliente
030200070124     c                   if        d10err <> ' '
030300070124     c                   eval      tfukun = tfuksc
030400070124     c                   eval      tfukund = tfukscd
030500070124      *
030600070124     c                   else
030700070124      *
030800070124     c                   eval      tfukun = d10cop
030900070124      * cerco i dati anagrafici
031000070124     c                   clear                   tibs69ds
031100070124     c                   move      tfukun        i69kac
031200070124     c                   move      tfukun        i69kcp
031300070124     c                   call      'TIBS69R'
031400070124     c                   parm                    tibs69ds
031500070124     c                   parm                    ds_cnaco
031600070124     c                   parm                    ds_cnind
031700070124     c                   parm                    ds_cnclp
031800070124     c                   parm                    ds_fncls
031900070124      *
032000070124     c                   eval      tfukund = acorag
032100070124
032200070124     c                   endif
032300110128
032400110128      * importanza cliente
032500110128      * messo in questo punto prende la categoria del cliente unificante
032600110128     c                   eval      tfuclv = clpclv
032700110128
032800070124      * verifico il codice commerciale unificante
032900130905     c     CLPage        chain     AZCMM000
033000130905     c                   if        not %found(AZCMM01L)
033100070124     c                   eval      tfucmu  = clpage
033200070124     c                   eval      tfucmud = *all'*'
033300070124     c                   else
033400130905     c                   eval      tfucmu  = CMMuni
033500130905     c     TFUcmu        chain     AZCMM000
033600130905     c                   if        not %found(AZCMM01L)
033700070124     c                   eval      tfucmud = *all'*'
033800070124     c                   else
033900130905     c                   eval      tfucmud = CMMdes
034000070124     c                   endif
034100070124     c                   endif
034200070124
034300070124      * albero commerciale unificante
034400070124     c                   movel     tfucmu        w0030
034500070124     c     w0030         chain     azorg01l
034600070124
034700070124      * distretto
034800070124     c                   eval      tfudis = orgfl3
034900070124     c                   eval      kcod = '17'
035000070124     c                   eval      kkey = tfudis
035100070124     c     ktabel        chain     tabel00f
035200070124     c                   if        not %found(tabel00f) or tblflg <> *blanks
035300070124     c                   eval      tfudisd = *all'*'
035400070124     c                   else
035500070124     c                   eval      tfudisd = tbluni
035600070124     c                   endif
035700070124      * area
035800070124     c                   eval      tfucar = orgcar
035900070124     c                   eval      kcod = '05'
036000070124     c                   movel     tfucar        kkey
036100070124     c     ktabel        chain     tabel00f
036200070124     c                   if        not %found(tabel00f) or tblflg <> *blanks
036300070124     c                   eval      tfucard = *all'*'
036400070124     c                   else
036500070124     c                   eval      tfucard = tbluni
036600070124     c                   endif
036700070124      * p.o.
036800070124     c                   eval      tfufil = orgfil
036900070124     c                   if        not %found(azorg01l) or orgfva <> *blanks
037000070124     c                   eval      tfufild = *all'*'
037100070124     c                   else
037200070124     c                   eval      tfufild = orgdes
037300070124     c                   endif
037400080922
037500080922      * imposto i dati relativi al fatturato
037600080922     c                   eval      tfudip = dsdip
037700080922     c                   eval      tfudfp = dsdfp
037800080922     c                   clear                   tise70ds
037900080922     c                   movel     'S'           i70tla
038000080922     c                   z-add     tfuksc        i70ksc
038100080922     c                   move      tfuctr        i70ctr
038200080922     c                   z-add     tfudip        i70dai
038300080922     c                   z-add     tfudfp        i70daf
038400080922     c                   call      'TISE70R'
038500080922     c                   parm                    tise70ds
038600080922     c                   z-add     o70ric        tfufat
038700080922     c                   z-add     o70rir        tfurir
038800080922     c                   z-add     o70rip        tfurip
038900110224
039000110224      /free
039100110224
039200110224       //?Cerco la data prezzo base di riferimento fuel su TITPT
039300140116       //?e % minima applicazione
039400110224        chain (TAMksc:TAMctr:TAMprg:TPDftc) TITPT01L;
039500110224        IF  %found(TITPT01L);
039600110224          TFUdpb = TPTdpb;
039700140116          dTPT01 = TPTflo;
039800140116          TFUpma = §TPTpma;
039900110224        ENDIF;
040000110224
040100110224       //?Con la data cerco il prezzo medio e lo scaglione su TIPMG
040200110224        setll TFUdpb TIPMG01L;
040300110224        read  TIPMG01L;
040400110224        IF  not %eof(TIPMG01L);
040500110224          TFUpmg = PMGpmg;
040600110224          TFUsca = PMGsca;
040700110224        ENDIF;
040800110224
040900110224      /end-free
041000110224
041100060215      * scrivo file
041200070124     c                   add       1             tottar
041300070123     c                   write     wftfu000
041400060320
041500060215
041600060215     c                   endsr
041700070124
041800060320      *--------------------------------------------------------------*
041900060320      * ROUTINE INIZIALE
042000060320      *--------------------------------------------------------------*
042100060320     c     *inzsr        begsr
042200060320
042300060320     c     *entry        plist
042400060320     c                   parm                    kpjba
042500080922     c                   movel     kpjbu         tnta86ds
042600060320
042700060320     c     *dtaara       define    §azute        azuteds
042800060320     c     *dtaara       define    §datiute      ddatiute
042900060320
043000060320     c                   in(e)     *dtaara
043100060320     c                   if        %error or rsut = *blanks
043200060320     c                   clear                   tibs34ds
043300060320     c                   call      'TIBS34R'
043400060320     c                   parm                    tibs34ds
043500060320     c                   in        *dtaara
043600060320     c                   endif
043700060320
043800060320      * se s.i. di prova imposto la libreria di prova
043900060320     c                   if        %subst(knsif:7:1) = 'P'
044000060320     c                   eval      wlib = 'GAITRAAZP '
044100060320     c                   else
044200060320     c                   eval      wlib = 'GAITRAAZM '
044300060320     c                   endif
044400060320
044500130403      * se lavoro lanciato da TISE01R imposto il wrkf a WFTFU20F
044600130403      * altrimenti lo imposto a WFTFU10F
044700130403     c                   if        ds10F = 'S'
044800130403     c                   eval      wLibWF = %trim(wLib) + '/WFTFU20F'
044900130403     c                   else
045000130403     c                   eval      wLibWF = %trim(wLib) + '/WFTFU10F'
045100130403     c                   endif
045200130403
045300070124      * pulisco il file
045400130403     c**!!!              clear                   comman
045500130403     c**!!!              movel(p)  cmd(1)        comman
045600130403     c**!!!              eval      %subst(comman:13:9) = wlib
045700130403     c                   eval      Comman = %trimr(Cmd(1)) +
045800130403     c                                      %trimr(wLibWF) + ')'
045900060320     c                   call      'QCMDEXC'
046000060320     c                   parm                    comman
046100060320     c                   parm                    lenght
046200070124
046300080925     c                   open      wftfu10f
046400060320
046500060320      * reperisco data e ora del lancio
046600060320     c                   time                    w0140
046700060320     c                   move      w0140         wdata
046800060320     c                   movel     w0140         wora
046900060320
047000060320
047100060320     c     ktpt          klist
047200060320     c                   kfld                    tamksc
047300060320     c                   kfld                    tamctr
047400060320     c                   kfld                    tamprg
047500060320     c                   kfld                    kftc
047600060320
047700060320     c     ktabel        klist
047800060320     c                   kfld                    codut
047900060320     c                   kfld                    kcod
048000060320     c                   kfld                    kkey
048100060320
048200060320     c                   endsr
048300060320      *--------------------------------------------------------------*
048400060320**   cmd - comando
048500130403CLRPFM FILE(
