000100150217     /*END
000200060320     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000300060320      *------------------------------------------------------------------------*
000400060320      *
000500070123      *  Stato avanzamento Istituto FUEL
000600060320      *
000700060320      *------------------------------------------------------------------------*
000800060215
000900060215     ftntam04l  if   e           k disk
001000060320     ftitpd01l  if   e           k disk
001100110224     fTITPT01L  if   e           k disk
001200110224     fTIPMG01L  if   e           k disk
001300150217     fSISDP01L  if   e           k disk
001400130905     fAZCMM01L  if   e           k disk
001500060320     fazorg01l  if   e           k disk
001600060320     ftabel00f  if   e           k disk
001700080925     fwftfu10f  o    e             disk    usropn
001800130403     f                                     extfile(wLibWF)
001900070124     ftnta77p   o    e             printer oflind(*in90)
002000060323
002100060323      *------------------------------------------------------------------------*
002200060323      *  RIEPILOGO INDICATORI
002300060323      *------------------------------------------------------------------------*
002400060323      * 02 - ho scritto almeno 1 rcd
002500060323      * 12 - utente di sede
002600060323      * 30 - comodo
002700060215
002800060215      *------------------------------------------------------------------------*
002900060215      *   V A R I A B I L I
003000060215      *------------------------------------------------------------------------*
003100060320     d comman          s             80
003200060320     d codut           s              1  0 inz(1)
003300060320     d kcod            s                   like(tblcod)
003400060320     d kksc            s                   like(tamksc)
003500060320     d kkey            s                   like(tblkey)
003600060320     d lenght          s             15  5 inz(80)
003700070124     d kftc            s                   like(tpdftc)
003800060215     d wctr            s                   like(tamctr)
003900060320     d wdata           s              8  0
004000080924     d wfd1            s                   like(tpditr)
004100080924     d wfd2            s                   like(tpditr)
004200080924     d wfd3            s                   like(tpditr)
004300080924     d wsg1            s                   like(tpdsgl)
004400080924     d wsg2            s                   like(tpdsgl)
004500080924     d wsg3            s                   like(tpdsgl)
004600080924     d wcts            s                   like(tpdcts)
004700060215     d wksc            s                   like(tamksc)
004800060320     d wlib            s              9
004900060320     d wora            s              6  0
005000060320     d w0030           s              3  0
005100060320     d w0140           s             14  0
005200070126     d contatore       s              2  0
005300080924     d overcts         s              1
005400130403
005500130403       // -?Nomi estesi libreria/file dei file?
005600130403     d wLibWF          s             21a   inz
005700060320
005800060320      *------------------------------------------------------------------------*
005900060320      *   S C H I E R E
006000060320      *------------------------------------------------------------------------*
006100070123     d cmd             s             80    dim(1) ctdata perrcd(1)
006200150217      *
006300150217      * -?Varie che fanno parte dell'Imponibile FUEL?
006400150217     d sk_SVR          s                   like(SDPsvr)
006500150217     d                                     dim(20)  ctdata  perrcd(1)
006600060215
006700060215      *------------------------------------------------------------------------*
006800060215      *   D S   I N T E R N E / E S T E R N E
006900060215      *------------------------------------------------------------------------*
007000080922     d tnta86ds        ds                  inz
007100080922     d  dsdip                         6  0
007200080922     d  dsdfp                         6  0
007300081008     d  dsdru                         8  0
007400130403     d  dsDIV                         8  0
007500130403     d  ds10F                         1
007600060321
007700060321     d wlbdat          ds                  inz
007800060321     d  g02dat                 1      8  0
007900060321     d  g02inv                 9     16  0
008000060321     d  g02err                17     17
008100060321     d  g02tgi                18     22  0
008200060320
008300060320     d azuteds       e ds                  extname(azute00f)
008400060320     d ddatiute      e ds
008500060320     d ds_cnaco      e ds                  inz  extname(Cnaco00f)
008600060320     d ds_cnind      e ds                  inz  extname(Cnind00f)
008700060320     d ds_cnclp      e ds                  inz  extname(Cnclp00f)
008800060320     d ds_fncls      e ds                  inz  extname(Fncls00f)
008900060215     d kpjba         e ds
009000060215     d dsta01        e ds
009100070124     d tibs10ds      e ds
009200060320     d tibs34ds      e ds
009300060320     d tibs69ds      e ds
009400080922     d tise70ds      e ds
009500140116
009600140116       // -?DS campo TPTflo
009700140116     d dTPT01        e ds
009800150217
009900150217       //--------------------------------------------------------------
010000150217       //?Definizione key-list.                                        ?
010100150217       //--------------------------------------------------------------
010200150217
010300150217       // -?File SISDP01L?
010400150217     d keySISDP01    e ds                  extname( SISDP01L : *key )
010500150217     d                                     prefix( k_ )   inz
010600060215
010700060215      *------------------------------------------------------------------------*
010800060320
010900060320     c                   clear                   wksc
011000060215     c                   eval      wctr = 999
011100060320     c                   move      *all'0'       kksc
011200060215
011300060320     c     kksc          setll     tntam04l
011400070124    1c                   do        *hival
011500060215     c                   read      tntam04l
011600060215      * fine file
011700060215     c                   if        %eof(tntam04l)
011800060215     c                   leave
011900060215     c                   endif
012000130403
012100130403      * Verifico data decorrenza e scadenza della tariffa con i dati passati
012200130403
012300130403      * se data decorrenza e scadenza della tariffa è maggiore della data
012400130403      * validità della tariffa leggo la successiva
012500130403     c                   if        tamddt > dsdiv and tamdst > dsdiv
012600130403     c                   iter
012700130403     c                   endif
012800060320
012900060320      * cambio tariffa
013000070124    2c                   if        tamctr <> wctr or tamksc <> wksc
013100060320
013200060320     c                   movel     tamflo        dsta01
013300060320
013400060320      * escludo le annullate
013500060215     c                   if        tamatb <> *blanks
013600060215     c                   iter
013700060215     c                   endif
013800060320      * escludo le bloccate
013900111221     c**!!!              if        tambap = 'B'
014000111221     c**!!!              iter
014100111221     c**!!!              endif
014200070123      * escludo le tariffe  FED
014300070123     c                   if        §tafed = 'S'
014400060320     c                   iter
014500060320     c                   endif
014600060320
014700060320      * cerco i dati anagrafici
014800060320     c                   clear                   tibs69ds
014900060320     c                   move      tamksc        i69kac
015000060320     c                   move      tamksc        i69kcp
015100060320     c                   call      'TIBS69R'
015200060320     c                   parm                    tibs69ds
015300060320     c                   parm                    ds_cnaco
015400060320     c                   parm                    ds_cnind
015500060320     c                   parm                    ds_cnclp
015600060320     c                   parm                    ds_fncls
015700060320      * escludo le tariffe dei clienti bloccati
015800130315     c                   if        acoabl <> *blanks
015900060320     c                   iter
016000060320     c                   endif
016100111221
016200111221      * solo se non bloccata
016300111221     c                   if        tambap <> 'B'
016400060320
016500070123      * controllo se Fuel presente
016600070123     c                   eval      kftc = 'f '
016700080924     c                   clear                   wfd1
016800080924     c                   clear                   wfd2
016900080924     c                   clear                   wfd3
017000080924     c                   clear                   wsg1
017100080924     c                   clear                   wsg2
017200080924     c                   clear                   wsg3
017300080924     c                   clear                   wcts
017400080924     c                   clear                   overcts
017500070126     c                   eval      contatore = 0
017600070123      * cerco importo percentuale
017700060320     c     ktpt          setll     titpd01l
017800070124    3c                   do        *hival
017900060320     c     ktpt          reade     titpd01l
018000060320      * fine file
018100060320     c                   if        %eof(titpd01l)
018200060320     c                   leave
018300060320     c                   endif
018400060320      * non annullate
018500060320     c                   if        tpdatb <> *blanks
018600060320     c                   iter
018700060320     c                   endif
018800080924      * se il codice tassazione è diverso da blank verifico se è lo stesso
018900080924      * altrimenti valorizzo il flag di + codici tassazione
019000080924     c                   If        wcts = *blank
019100080924     c                   eval      wcts = tpdcts
019200080924     c                   endif
019300080924
019400080924     c                   if        wcts <> tpdcts
019500080924     c                   eval      overcts = 'S'
019600080924     c                   leave
019700080924     c                   endif
019800080924      * scaglioni
019900070126     c                   add       1             contatore
020000080924      * uniformo lo scaglione 99999 tutti quelli che lo scaglione maggiore 99
020100080924     c                   if        tpdsgl >= 99
020200080924     c                   eval      tpdsgl = 99999,000
020300080924     c                   endif
020400080924
020500070126     c                   if        contatore = 1
020600080924     c                   eval      wfd1 = tpditr
020700080924     c                   eval      wsg1 = tpdsgl
020800080924     c                   iter
020900080924     c                   endif
021000080924
021100080924     c                   if        contatore = 2
021200080924     c                   eval      wfd2 = tpditr
021300080924     c                   eval      wsg2 = tpdsgl
021400080924     c                   iter
021500080924     c                   endif
021600080924
021700080924     c                   if        contatore = 3
021800080924     c                   eval      wfd3 = tpditr
021900080924     c                   eval      wsg3 = tpdsgl
022000080924     c                   iter
022100080924     c                   endif
022200080924
022300080924     c                   if        contatore > 3
022400060320     c                   leave
022500070124     c                   endif
022600080924
022700070124    3c                   enddo
022800070123      * scrivo il file
022900060320     c                   exsr      scrivi
023000111221     c                   endif
023100060215
023200060320     c                   eval      wksc = tamksc
023300060320     c                   eval      wctr = tamctr
023400060215
023500060215      * se la tariffa che sto leggendo deve ancora andare in vigore la elaboro
023600070124      * ma devo elaborare anche la tariffa precedente che è in vigore ora
023700130403      * => In realtà ora considero la data (ricevuta come parametro) in cui
023800130403      *    entra in vigore la tariffa - non più la data odierna
023900130403     c**!!!              if        tamddt > *date
024000130403     c**!!!              eval      wctr = 999
024100130403     c**!!!              endif
024200070124    2c                   endif
024300060215
024400060320    1c                   enddo
024500060320
024600070124      * stampo  la fine elaborazione
024700070124     c                   write     ta77s1
024800060215
024900060215     c                   eval      *inlr = *on
025000060215
025100060215      *------------------------------------------------------------------------*
025200060320      * SCRIVO IL FILE PER LA STAMPA
025300060215      *------------------------------------------------------------------------*
025400060215     c     scrivi        begsr
025500060215
025600060323     c  n02              eval      *in02 = *on
025700060323
025800070123     c                   clear                   wftfu000
025900060320
026000060320      * utente/data/ora elaborazione
026100070123     c                   eval      tfupru = knmus
026200070123     c                   eval      tfudte = wdata
026300070123     c                   eval      tfuore = wora
026400060215      * cliente
026500070123     c                   eval      tfuksc  = tamksc
026600070123     c                   eval      tfukscd = acorag
026700060215      * data ultima spedizione fattura
026800080923     c                   if        clpdus > *zeros
026900080923      * controllo la data lunga 8
027000080923     c                   clear                   wlbdat
027100080923     c                   z-add     clpdus        g02inv
027200080923     c                   eval      g02err = '3'
027300080923     c                   call      'XSRDA8'
027400080923     c                   parm                    wlbdat
027500080923     c                   eval      tfudus = g02inv
027600080923     c                   endif
027700060215      * tariffa
027800070123     c                   eval      tfuctr = tamctr
027900070123     c                   eval      tfuprg = tamprg
028000070123     c                   eval      tfuddt = tamddt
028100070123     c                   eval      tfudst = tamdst
028200070904      * tipo tariffa
028300070904     c                   select
028400070904     c                   when      §tadpd = 'S'
028500070904     c                   eval      tfutpt = 'D'
028600070904     c                   when      §tafed = 'S'
028700070904     c                   eval      tfutpt = 'F'
028800070904     c                   other
028900070904     c                   eval      tfutpt = tamfie
029000070904     c                   endsl
029100080924      * Fattore demoltiplicatore per ogni scaglione
029200080924     c                   eval      tfufd1 = wfd1
029300080924     c                   eval      tfusg1 = wsg1
029400080924     c                   eval      tfufd2 = wfd2
029500080924     c                   eval      tfusg2 = wsg2
029600080924     c                   eval      tfufd3 = wfd3
029700080924     c                   eval      tfusg3 = wsg3
029800080924      * se ci sono più codici tassazione valorizzo il flag
029900080924     c                   if        overcts = 'S'
030000080924     c                   eval      tfufct = 'S'
030100080924     c                   endif
030200080924      * se ci sono più scaglioni per lo stesso codice tassazione
030300080924     c                   if        contatore > 3
030400080924     c                   eval      tfufsg = 'S'
030500080924     c                   endif
030600070124      * cerco il codice cliente unificante
030700070124     c                   clear                   tibs10ds
030800070124     c                   eval      d10tle = 'ST'
030900070124     c                   eval      d10paf = 'P'
031000070124     c                   z-add     tfuksc        d10cod
031100081008     c                   z-add     dsdru         d10drf
031200070124     c                   CALL      'TIBS10R'
031300070124     c                   parm                    tibs10ds
031400070124     c                   parm      'A'           partpexe          1
031500070124      * se ci sono errori valorizzo come unificante il cliente
031600070124     c                   if        d10err <> ' '
031700070124     c                   eval      tfukun = tfuksc
031800070124     c                   eval      tfukund = tfukscd
031900070124      *
032000070124     c                   else
032100070124      *
032200070124     c                   eval      tfukun = d10cop
032300070124      * cerco i dati anagrafici
032400070124     c                   clear                   tibs69ds
032500070124     c                   move      tfukun        i69kac
032600070124     c                   move      tfukun        i69kcp
032700070124     c                   call      'TIBS69R'
032800070124     c                   parm                    tibs69ds
032900070124     c                   parm                    ds_cnaco
033000070124     c                   parm                    ds_cnind
033100070124     c                   parm                    ds_cnclp
033200070124     c                   parm                    ds_fncls
033300070124      *
033400070124     c                   eval      tfukund = acorag
033500070124
033600070124     c                   endif
033700110128
033800110128      * importanza cliente
033900110128      * messo in questo punto prende la categoria del cliente unificante
034000110128     c                   eval      tfuclv = clpclv
034100110128
034200070124      * verifico il codice commerciale unificante
034300130905     c     CLPage        chain     AZCMM000
034400130905     c                   if        not %found(AZCMM01L)
034500070124     c                   eval      tfucmu  = clpage
034600070124     c                   eval      tfucmud = *all'*'
034700070124     c                   else
034800130905     c                   eval      tfucmu  = CMMuni
034900130905     c     TFUcmu        chain     AZCMM000
035000130905     c                   if        not %found(AZCMM01L)
035100070124     c                   eval      tfucmud = *all'*'
035200070124     c                   else
035300130905     c                   eval      tfucmud = CMMdes
035400070124     c                   endif
035500070124     c                   endif
035600070124
035700070124      * albero commerciale unificante
035800070124     c                   movel     tfucmu        w0030
035900070124     c     w0030         chain     azorg01l
036000070124
036100070124      * distretto
036200070124     c                   eval      tfudis = orgfl3
036300070124     c                   eval      kcod = '17'
036400070124     c                   eval      kkey = tfudis
036500070124     c     ktabel        chain     tabel00f
036600070124     c                   if        not %found(tabel00f) or tblflg <> *blanks
036700070124     c                   eval      tfudisd = *all'*'
036800070124     c                   else
036900070124     c                   eval      tfudisd = tbluni
037000070124     c                   endif
037100070124      * area
037200070124     c                   eval      tfucar = orgcar
037300070124     c                   eval      kcod = '05'
037400070124     c                   movel     tfucar        kkey
037500070124     c     ktabel        chain     tabel00f
037600070124     c                   if        not %found(tabel00f) or tblflg <> *blanks
037700070124     c                   eval      tfucard = *all'*'
037800070124     c                   else
037900070124     c                   eval      tfucard = tbluni
038000070124     c                   endif
038100070124      * p.o.
038200070124     c                   eval      tfufil = orgfil
038300070124     c                   if        not %found(azorg01l) or orgfva <> *blanks
038400070124     c                   eval      tfufild = *all'*'
038500070124     c                   else
038600070124     c                   eval      tfufild = orgdes
038700070124     c                   endif
038800080922
038900080922      * imposto i dati relativi al fatturato
039000080922     c                   eval      tfudip = dsdip
039100080922     c                   eval      tfudfp = dsdfp
039200080922     c                   clear                   tise70ds
039300080922     c                   movel     'S'           i70tla
039400080922     c                   z-add     tfuksc        i70ksc
039500080922     c                   move      tfuctr        i70ctr
039600080922     c                   z-add     tfudip        i70dai
039700080922     c                   z-add     tfudfp        i70daf
039800080922     c                   call      'TISE70R'
039900080922     c                   parm                    tise70ds
040000080922     c                   z-add     o70ric        tfufat
040100080922     c                   z-add     o70rir        tfurir
040200080922     c                   z-add     o70rip        tfurip
040300110224
040400110224      /free
040500110224
040600110224       //?Cerco la data prezzo base di riferimento fuel su TITPT
040700150217       //?e % minima applicazione e flag "Applicazione VMA"
040800110224        chain (TAMksc:TAMctr:TAMprg:TPDftc) TITPT01L;
040900110224        IF  %found(TITPT01L);
041000110224          TFUdpb = TPTdpb;
041100140116          dTPT01 = TPTflo;
041200140116          TFUpma = §TPTpma;
041300150217          TFUvma = §TPTvma;
041400110224        ENDIF;
041500110224
041600110224       //?Con la data cerco il prezzo medio e lo scaglione su TIPMG
041700110224        setll TFUdpb TIPMG01L;
041800110224        read  TIPMG01L;
041900110224        IF  not %eof(TIPMG01L);
042000110224          TFUpmg = PMGpmg;
042100110224          TFUsca = PMGsca;
042200110224        ENDIF;
042300150217
042400150217        // -?Calcolo Varia FUEL e Imponibile Varia FUEL?
042500150217        clear  keySISDP01;
042600150217        k_SDPksc = TAMksc;
042700150217        k_SDPann = dsDIP / 100;
042800150217        k_SDPmes = dsDIP - ( k_SDPann * 100 );
042900150217        k_SDPctr = TAMctr;
043000150217        //k_SDPsvr = 'f ';
043100150217        setll  %kds( keySISDP01 )  SISDP000;
043200150217        reade  %kds( keySISDP01 : 1 )  SISDP000;
043300150217        DoW  Not %eof(SISDP01L);
043400150217          if  ( SDPann * 100 ) + SDPmes >= dsDIP  and
043500150217              ( SDPann * 100 ) + SDPmes <= dsDFP  and
043600150217              SDPctr = TAMctr;
043700150217            select;
043800150217              // -?Varia FUEL?
043900150217              when  SDPsvr = 'f';
044000150217                TFUvaf += SDPrcp;
044100150217              // -?Imponibile Varia FUEL?
044200150217              when  %lookup( SDPsvr : sk_SVR ) > *zero;
044300150217                TFUivf += SDPrcp;
044400150217            endsl;
044500150217          endif;
044600150217          reade  %kds( keySISDP01 : 1 )  SISDP000;
044700150217        EndDo;
044800110224
044900110224      /end-free
045000110224
045100060215      * scrivo file
045200070124     c                   add       1             tottar
045300070123     c                   write     wftfu000
045400060320
045500060215
045600060215     c                   endsr
045700070124
045800060320      *--------------------------------------------------------------*
045900060320      * ROUTINE INIZIALE
046000060320      *--------------------------------------------------------------*
046100060320     c     *inzsr        begsr
046200060320
046300060320     c     *entry        plist
046400060320     c                   parm                    kpjba
046500080922     c                   movel     kpjbu         tnta86ds
046600060320
046700060320     c     *dtaara       define    §azute        azuteds
046800060320     c     *dtaara       define    §datiute      ddatiute
046900060320
047000060320     c                   in(e)     *dtaara
047100060320     c                   if        %error or rsut = *blanks
047200060320     c                   clear                   tibs34ds
047300060320     c                   call      'TIBS34R'
047400060320     c                   parm                    tibs34ds
047500060320     c                   in        *dtaara
047600060320     c                   endif
047700060320
047800060320      * se s.i. di prova imposto la libreria di prova
047900060320     c                   if        %subst(knsif:7:1) = 'P'
048000060320     c                   eval      wlib = 'GAITRAAZP '
048100060320     c                   else
048200060320     c                   eval      wlib = 'GAITRAAZM '
048300060320     c                   endif
048400060320
048500130403      * se lavoro lanciato da TISE01R imposto il wrkf a WFTFU20F
048600130403      * altrimenti lo imposto a WFTFU10F
048700130403     c                   if        ds10F = 'S'
048800130403     c                   eval      wLibWF = %trim(wLib) + '/WFTFU20F'
048900130403     c                   else
049000130403     c                   eval      wLibWF = %trim(wLib) + '/WFTFU10F'
049100130403     c                   endif
049200130403
049300070124      * pulisco il file
049400130403     c                   eval      Comman = %trimr(Cmd(1)) +
049500130403     c                                      %trimr(wLibWF) + ')'
049600060320     c                   call      'QCMDEXC'
049700060320     c                   parm                    comman
049800060320     c                   parm                    lenght
049900070124
050000080925     c                   open      wftfu10f
050100060320
050200060320      * reperisco data e ora del lancio
050300060320     c                   time                    w0140
050400060320     c                   move      w0140         wdata
050500060320     c                   movel     w0140         wora
050600060320
050700060320
050800060320     c     ktpt          klist
050900060320     c                   kfld                    tamksc
051000060320     c                   kfld                    tamctr
051100060320     c                   kfld                    tamprg
051200060320     c                   kfld                    kftc
051300060320
051400060320     c     ktabel        klist
051500060320     c                   kfld                    codut
051600060320     c                   kfld                    kcod
051700060320     c                   kfld                    kkey
051800060320
051900060320     c                   endsr
052000060320      *--------------------------------------------------------------*
052100060320**   cmd - comando
052200130403CLRPFM FILE(
052300150217** -?sk_SVR:?Varie che fanno parte dell'imponibile Fuel?
052400150217=  Reso bancali
052500150217c  Lasciato avviso
052600150217e  Espresso/Priority
052700150217h  H 10:30
052800150217A  Appuntamento
052900150217B  Consegna DDT
053000150217C  Facchinaggio arr.
053100150217F  Fuori misura
053200150217H  Diritto fisso
053300150217I  Spese di giacenza
053400150217J  Isola
053500150217K  Località disagiata
053600150217N  Anteporto
053700150217P  Ai piani
053800150217Q  Centro Storico
053900150217S  Supermercato
054000150217U  Ritiro
054100150217X  Consegna disagiata
0542001502171  Porto
0543001502172  Inoltro
