000100050412     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200050408
000300050408      *------------------------------------------------------------------------*
000400050408      *
000500050408      *  Archiviazione C.A. chiuse
000600050408      *
000700050408      *------------------------------------------------------------------------*
000800050408
000900050415     fCnabi01l  if   e           k disk
001000050415     fFndcc01l  if   e           k disk
001100050415     fFndcf01l  if   e           k disk
001200050415     fFndcl01l  if   e           k disk
001300050415     fFndcs01l  if   e           k disk
001400050504     fFndct01l  if   e           k disk
001500050415     fFndet01l  if   e           k disk
001600050428     fFndet02l  if   e           k disk    rename(fndet000:fndet02)
001700050415     fFndfa01l  if   e           k disk
001800050415     fFndka01L  if   e           k disk
001900050414     fFndpt01l  if   e           k disk
002000050415     fTabel00f  if   e           k disk
002100050415     fTitas30c  if   e           k disk
002200050428     fTita430c  if   e           k disk
002300050512     fTntmd01l  if   e           k disk
002400150617     fFidn91p   o    e             printer  oflind(*in98) usropn
002500150617     f                                      infds(prtline)
002600050408
002700050408      *------------------------------------------------------------------------*
002800050408      *  RIEPILOGO INDICATORI
002900050408      *------------------------------------------------------------------------*
003000050517      * 01   - Stampo Rivalsa
003100050517      * 02   - Stampo Responsabilità Internazionale
003200050517      * 03   - Stampo Importo da assicurare
003300050517      * 04   - Stampo Evento
003400050517      * 05   - Stampo Mandato
003500050517      * 06   - Stampo Note calcolo prg. liquidazione
003600050517      * 07   - Non stampo data/ora immissione e la fase nota generica
003700070104      * 08   - Non stampo data ricezione disposizioni A.I.G.
003800050415      * 10   - Stampo la c.a. passata in kpjbu
003900050415      * 20   - Liquidazione transattiva
004000050415      * 21   - Pratica
004100050415      * 22   - Pratica in franchigia
004200050413      * 98   - Overflow
004300050412      * 99   - comodo
004400050504
004500050504      *------------------------------------------------------------------------*
004600050504      *   S C H I E R E
004700050504      *------------------------------------------------------------------------*
004800050509     d s_rigadoc       s             74    dim(500)
004900050516     d s_rigafase      s             97    dim(500)
005000050408
005100050408      *------------------------------------------------------------------------*
005200050408      *   V A R I A B I L I
005300050408      *------------------------------------------------------------------------*
005400050411     d codut           s              1  0 inz(1)
005500050504     d col1            s              3  0
005600050504     d col2            s              3  0
005700050412     d conta           s              3  0
005800050503     d datadc          s                   like(dclddc)
005900050504     d datafa          s                   like(dclddc)
006000050516     d datanot         s                   like(dcsdim)
006100050509     d datarz          s                   like(dclddc)
006200050411     d flgtb1          s                   like(§tbtpo)
006300050412     d kdcsdim         s                   like(DcsDim)
006400050412     d kdcshim         s                   like(DcsHim)
006500050412     d kdcsnks         s                   like(DcsNks)
006600050412     d kdcsnkt         s                   like(DcsNkt)
006700050414     d kdcspno         s                   like(DcsPno)
006800050412     d kdcsstd         s                   like(DcsStd)
006900050412     d kdcstpd         s                   like(DcsTpd)
007000050412     d kdcstrc         s                   like(DcsTrc)
007100050412     d ktblcod         s                   like(TblCod)
007200050412     d ktblkey         s                   like(TblKey)
007300050428     d ktrc            s                   like(ta4trc)
007400150617     d NrCA            s              5  0
007500050517     d oldhim          s                   like(dcshim)
007600050504     d resto           s              3  0
007700050504     d righe           s              3  0
007800050414     d wabi            s                   like(AbiAbi)
007900050414     d wcab            s                   like(AbiCab)
008000050414     d wdataeur        s               d   datfmt(*eur)
008100050412     d wdcsnot         s                   like(DcsNot)
008200050503     d wdoc            s              1    inz('0')
008300050415     d wgiro           s              3  0 inz
008400050415     d wnote           s              1    inz('0')
008500050518     d wover           s              1    inz('0')
008600050412     d wtrc            s                   like(DkaTrc)
008700050419     d w001a           s              1
008800050412     d w0040           s              4  0
008900050412     d w0080           s              8  0
009000050510     d w014a           s             14
009100050408
009200050408      *------------------------------------------------------------------------*
009300050408      *   D S   I N T E R N E / E S T E R N E
009400050408      *------------------------------------------------------------------------*
009500050408     d parm01          ds
009600050516     d  waaca                         4  0
009700050418     d  wflg                          1
009800050414     d  waac                          4  0
009900050414     d  wfil                          3  0
010000050414     d  wnca                          7  0
010100050413
010200050413     d prtline         ds
010300050413     d  curline              367    368i 0
010400050412
010500050412     d                 ds
010600050412     d  dctaac                 1      4  0
010700050412     d  dctfil                 5      7  0
010800050412     d  dctnca                 8     14  0
010900050412     d  dsnumca                1     14  0
011000050414
011100050428     d                 ds
011200050503     d  wdctaac                1      4  0
011300050428     d  dctmgc                 5      8  0
011400050428     d  wdataca                1      8  0
011500050428
011600050414     d wlbdat          ds                  inz
011700050414     d  g02dat                 1      8  0
011800050414     d  g02inv                 9     16  0
011900050414     d  g02err                17     17
012000050414     d  g02tgi                18     22  0
012100050408
012200050412     d azuteds       e ds                  extname(AZUTE00F)
012300050412     d dcch          e ds
012400050414     d dcdd          e ds
012500050412     d ddatiute      e ds
012600050414     d ddcf01        e ds
012700050411     d ddct01        e ds
012800050411     d dstb          e ds
012900050414     d dstr          e ds
013000050414     d ds1p          e ds
013100050411     d dtad          e ds
013200050428     d dta4a         e ds
013300050412     d ds_cnaco      e ds                  extname(CNACO00F)
013400050412     d ds_cnind      e ds                  extname(CNIND00F)
013500050412     d ds_cnclp      e ds                  extname(CNCLP00F)
013600050412     d ds_fncls      e ds                  extname(FNCLS00F)
013700050408     d kpjba         e ds
013800050412     d tibs02ds      e ds
013900050412     d tibs34ds      e ds                  inz
014000050412     d tibs69ds      e ds
014100050408
014200050408      *------------------------------------------------------------------------*
014300050414
014400050414     c   10kwndc         Setll     Fndct01l
014500050516     c  n10waaca         Setll     Fndct01l
014600050411     c                   Do        *Hival
014700050414     c   10kwndc         Reade     Fndct01l
014800050516     c  n10waaca         Reade     Fndct01l
014900050415
015000050411      * Fine file
015100050516     c                   If        %Eof(Fndct01l)
015200050411     c                   Leave
015300050411     c                   EndIf
015400050503
015500050503     c                   Eval      wdctaac = dctaac
015600050415
015700050415      * Pulizia dei record di stampa
015800050415     c                   ExSr      Pulisci
015900050415
016000050415      * Valorizzo i flag operativi
016100050415     c                   Eval      ddct01 = dctflo
016200150617
016300150617      /free
016400150618       //?Se supero le 25000 c.a. chiudo e riapro il printer file
016500150617         NrCA += 1;
016600150618         IF  NrCA > 25000;
016700150617           close FIDN91P;
016800150617           open  FIDN91P;
016900150617           NrCA = 1;
017000150617         ENDIF;
017100150617      /end-free
017200050412
017300050415      * Stampo una c.a. x pagina
017400050606     c                   Clear                   page
017500050517     c                   Add       1             curline
017600050413     c                   Write     dn91t1
017700050411
017800050415      * Recupero i dati e stampo il dettaglio della c.a.
017900050411     c                   ExSr      Elabora
018000050411
018100050411     c                   EndDo
018200050411
018300150617      /free
018400150617       //?Chiudo printer file
018500150617         IF  %open(FIDN91P);
018600150617           close FIDN91P;
018700150617         ENDIF;
018800150617      /end-free
018900050411     c                   Eval      *InLr = *On
019000050414
019100050411      *------------------------------------------------------------------------*
019200050411      * ELABORO LA C.A.
019300050411      *------------------------------------------------------------------------*
019400050411     c     Elabora       BegSr
019500050415
019600050429      *
    C.A.                               ?
019700050415     c                   Eval      pdctfil = dctfil
019800050415     c                   Eval      pdctnca = dctnca
019900050415      * Data della c.a.
020000050412     c                   Move      dctmgc        w0080
020100050412     c                   Movel     dctaac        w0080
020200050414     c     *iso          Move      w0080         wdataeur
020300050414     c     *eur          Movel     wdataeur      dataca
020400050415
020500050429      *
    Pratica assicurativa               ?
020600050415     c                   Eval      pdctdit = dctdit
020700050415     c                   Eval      pdctpra = dctpra
020800050415     c                   Eval      pdctprn = dctprn
020900050429
021000050429      *
    Tipo gestione                      ?
021100050429     c                   Eval      *In20 = *Off
021200050429     c                   Eval      *In21 = *Off
021300050429     c                   Eval      *In22 = *Off
021400050429     c                   Select
021500050429     c                   When      dctfpr = 'T'
021600050429     c                   Eval      desfpr = 'LIQUIDAZIONE TRANSATTIVA'
021700050429     c                   Eval      tdespag = 'ESTREMI PAGAMENTO L.T.:'
021800050429     c                   Eval      *In20 = *On
021900050429     c                   When      dctfpr = 'P' and §dctpfra <> 'F'
022000050429     c                   Eval      desfpr = 'PRATICA ASSICURATIVA'
022100050429     c                   Eval      tdespag = 'ESTREMI PAGAMENTO PRATICA:'
022200050429     c                   Eval      *In21 = *On
022300050429     c                   When      dctfpr = 'P' and §dctpfra = 'F'
022400050429     c                   Eval      desfpr = 'PRATICA ASSICURAT.IN FRANCHIGIA'
022500050429     c                   Eval      tdespag = 'ESTREMI PAGAMENTO PRATICA IN FRAN-
022600050429     C                             CHIGIA:'
022700050429     c                   Eval      *In22 = *On
022800050429     c                   Other
022900050429     c                   Eval      tdespag = 'ESTREMI PAGAMENTO:'
023000050429     c                   EndSl
023100050429      * Gestione con Mittente / Destinatario
023200050429     c                   If        dctptr = 'M'
023300050429     c                   Eval      desfpr = %trim(desfpr) + ' con MITTENTE'
023400050429     c                   EndIf
023500050429     c                   If        dctptr = 'D'
023600050429     c                   Eval      desfpr = %trim(desfpr) + ' con DESTINATARIO'
023700050429     c                   EndIf
023800050415
023900050429      *
    Denuncia                           ?
024000050415     c                   Eval      pdctaad = dctaad
024100050415     c                   Eval      pdctpod = dctpod
024200050415     c                   Eval      pdctnde = dctnde
024300050428      * cerco la data di stampa della denuncia
024400050428     c                   If        dctnde > *Zeros
024500050428     c     kFndet2       Chain     Fndet02l
024600050428     c                   If        %Found(Fndet02l)
024700050428      * data di stampa della denuncia
024800050428     c                   Move      detdsb        w0080
024900050428     c     *iso          Move      w0080         wdataeur
025000050428     c     *eur          Movel     wdataeur      datade
025100050428     c                   EndIf
025200050428     c                   EndIf
025300050415
025400050428     c                   Eval      *In04 = *Off
025500050428
025600050429      *
    Evento                             ?
025700050415     c                   If        dctnev > *Zeros
025800050428     c                   Eval      *In04 = *On
025900050415     c                   Eval      pdctaae = dctaae
026000050415     c                   Eval      pdctnev = dctnev
026100050415      * reperisco i dati relativi all'evento
026200050415     c     kFndet        Chain     Fndet01l
026300050415     c                   If        %Found(Fndet01l)
026400050415      * data dell'evento
026500050415     c                   Move      detmge        w0080
026600050415     c                   Movel     detaae        w0080
026700050415     c     *iso          Move      w0080         wdataeur
026800050415     c     *eur          Movel     wdataeur      dataev
026900050415      * tipo anomalia evento
027000050415     c                   Clear                   dtad
027100050415     c                   Clear                   tibs02ds
027200050415     c                   Eval      t02mod = 'C'
027300050415     c                   Eval      t02sif = knsif
027400050415     c                   Eval      t02cod = 'TAD'
027500050415     c                   Eval      t02ke1 = dettad
027600050415     c                   Call      'TIBS02R'
027700050415     c                   Parm                    kpjba
027800050415     c                   Parm                    tibs02ds
027900050415     c                   If        t02err = *Blanks
028000050415     c                   Eval      dtad = t02uni
028100050415     c                   EndIf
028200050415     c                   Eval      destev = §taddesc
028300050415      * descrizione aggiuntiva evento
028400050415     c                   Eval      kdcstpd = 'C'
028500050415     c                   Movel(p)  dsnumca       kdcsnkt
028600050415     c                   Eval      kdcsstd = 'E'
028700050415     c                   Clear                   kdcsdim
028800050415     c                   Clear                   kdcshim
028900050415     c                   Clear                   kdcsnks
029000050415     c                   Eval      kdcstrc = 'D'
029100050415     c     kFndcs        Chain     Fndcs01l
029200050415     c                   If        %Found(Fndcs01l)
029300050415     c                   Eval      deseva = dcsnot
029400050415     c                   EndIf
029500050415     c                   EndIf
029600050415     c                   EndIf
029700050415
029800050429      *
    Spedizione                         ?
029900050415     c     kTitas        Chain     Tntmd01l
030000090122     c                   if        not %found(tntmd01l)
030100090122     c                   clear     *all          tntmd000
030200090122     c                   endif
030300050415     c     kTitas        Chain     Titas30c
030400090122     c                   if        not %found(titas30c)
030500090122     c                   clear     *all          titas000
030600090122     c                   endif
030700050415     c                   ExSr      Ctrtbl
030800050415     c                   If        flgtb1 = *Blanks
030900050415     c     kTitas        Reade     Titas30c
031000050415     c                   EndIf
031100050415      * mittente/destinario
031200050415     c                   If        §dctport = 'A'
031300050415     c                   Eval      climit = tasccm
031400050415     c                   Eval      clides = tasksc
031500050415     c                   Else
031600050415     c                   Eval      climit = tasksc
031700050415     c                   Clear                   clides
031800050415     c                   EndIf
031900050415      * indirizzo mittente
032000050415     c                   Eval      ptmdrsm = tmdrsm
032100050516     c                   Eval      deslom = %trim(tmdinm) + ' ' + '-' + ' ' +
032200050516     c                                      %trim(tmdcam) + ' ' + %trim(tmdlom)
032300050516     c                                      + '  ' + tmdprm + ' ' + tmdnzm
032400050415      * indirizzo destinatario
032500050415     c                   Eval      ptmdrsd = tmdrsd
032600050516     c                   Eval      deslod = %trim(tmdind) + ' ' + '-' + ' ' +
032700050516     c                                      %trim(tmdcad) + ' ' + %trim(tmdlod)
032800050516     c                                      + '  ' + tmdprd + ' ' + tmdnzd
032900050415      * numero spedizone
033000050415     c                   Eval      pdctlnp = dctlnp
033100050415     c                   Eval      pdctnrs = dctnrs
033200050415     c                   Eval      pdctnsp = dctnsp
033300050415      * data della spedizione
033400050415     c                   Move      tasmgs        w0080
033500050415     c                   Movel     tasaas        w0080
033600090331      * se data a 0 pulisco il campo data spedizione
033700090331     c                   if        w0080 = *zeros
033800090331     c                   clear                   datasp
033900090331     c                   else
034000050415     c     *iso          Move      w0080         wdataeur
034100050415     c     *eur          Movel     wdataeur      datasp
034200090331     c                   endif
034300050415      * tipo bolla assegnato/franco
034400050411     c                   If        §dctport = 'F'
034500050411     c                   Eval      descbo = 'FRANCO'
034600050411     c                   Else
034700050411     c                   Eval      descbo = 'ASSEGNATO'
034800050411     c                   EndIf
034900050415      * linea di arrivo
035000050415     c                   Eval      pdctlna = dctlna
035100050415      * riferimento mittente
035200050415     c                   Eval      ptasrmn = tasrmn
035300050428      * riferimento mittente alfanumerico
035400050428     c                   Eval      ktrc = 'A'
035500050428     c     ktita4        Chain     Tita430c
035600050428     c                   If        %Found(Tita430c)
035700050428     c                   Eval      dta4a = ta4not
035800050428     c                   Eval      pta4rma = §ta4arma
035900050428     c                   EndIf
036000050415      * colli/peso/volume
036100050415     c                   Eval      ptasncl = tasncl
036200050415     c                   Eval      ptaspkf = taspkf
036300050415     c                   Eval      ptasfvf = tasfvf
036400050415     c                   Eval      ptasvlf = tasvlf
036500050415      * natura merce
036600050415     c                   Eval      pdctnas = dctnas
036700050415
036800050415      * --> STAMPO 1° record
036900050517     c  n04              Add       5             curline
037000050517     c   04              Add       6             curline
037100050415     c                   Write     dn91d1
037200050412
037300050429      *
    Anomalia                           ?
037400050418     c                   Eval      pdcttad = dcttad
037500050415      * decodifico
037600050412     c                   Clear                   dtad
037700050412     c                   Clear                   tibs02ds
037800050412     c                   Eval      t02mod = 'C'
037900050412     c                   Eval      t02sif = knsif
038000050412     c                   Eval      t02cod = 'TAD'
038100050412     c                   Eval      t02ke1 = dcttad
038200050412     c                   Call      'TIBS02R'
038300050412     c                   Parm                    kpjba
038400050412     c                   Parm                    tibs02ds
038500050412     c                   If        t02err = *Blanks
038600050412     c                   Eval      dtad = t02uni
038700050412     c                   EndIf
038800050415     c                   Eval      destad = §taddesc
038900050415     c                   Eval      wnote = *Off
039000050415      * cerco le descrizioni aggiuntive
039100050412     c                   Eval      kdcstpd = 'C'
039200050412     c                   Movel(p)  dsnumca       kdcsnkt
039300050412     c                   Eval      kdcsstd = 'A'
039400050412     c                   Clear                   kdcsdim
039500050412     c                   Clear                   kdcshim
039600050412     c                   Clear                   kdcsnks
039700050412     c                   Eval      kdcstrc = 'D'
039800050415     c                   Eval      conta = 1
039900050415     c                   Eval      wgiro = 1
040000050415     c     kFndcs        Setll     Fndcs01l
040100050415     c                   Do        *Hival
040200050415     c     kFndcs        Reade     Fndcs01l
040300050415     c                   If        %Eof(Fndcs01l)
040400050415     c                   Leave
040500050415     c                   EndIf
040600050415     c                   Eval      wnote = *On
040700050415     c                   Select
040800050415      * se primo giro devo stampare 4 note assieme
040900050415     c                   When      wgiro = 1
041000050415      * se ho già caricato 4 note salvo la chiave della nota che sto leggendo x poi riposizionarmi
041100050415     c                   If        conta = 5
041200050415     c                   Eval      kdcstpd = dcstpd
041300050415     c                   Eval      kdcsnkt = dcsnkt
041400050415     c                   Eval      kdcsstd = dcsstd
041500050415     c                   Eval      kdcsdim = dcsdim
041600050415     c                   Eval      kdcshim = dcshim
041700050415     c                   Eval      kdcsnks = dcsnks
041800050415     c                   Eval      kdcstrc = dcstrc
041900050415     c                   Eval      kdcspno = dcspno
042000050415      * --> STAMPO 2° record
042100050517     c                   Eval      destag = %trim(destag)
042200050517     c                   Add       1             curline
042300050415     c                   Write     dn91d2
042400050415      * riporto il contatore a 1
042500050415     c                   Eval      conta = 1
042600050415     c                   Eval      wgiro = 2
042700050415      * mi riposiziono sul file delle note all'ultima nota letta e torno a leggere
042800050415     c     kFndcs01      Setll     Fndcs01l
042900050415     c                   Iter
043000050415     c                   EndIf
043100050428      * se non è il primo giro devo stampare 4 note assieme
043200050415     c                   Other
043300050428      * se ho già caricato 4 note salvo la chiave della nota che sto leggendo x poi riposizionarmi
043400050428     c                   If        conta = 5
043500050415     c                   Eval      kdcstpd = dcstpd
043600050415     c                   Eval      kdcsnkt = dcsnkt
043700050415     c                   Eval      kdcsstd = dcsstd
043800050415     c                   Eval      kdcsdim = dcsdim
043900050415     c                   Eval      kdcshim = dcshim
044000050415     c                   Eval      kdcsnks = dcsnks
044100050415     c                   Eval      kdcstrc = dcstrc
044200050415     c                   Eval      kdcspno = dcspno
044300050415      * --> STAMPO 3° record
044400050517     c                   Eval      desta1 = %trim(desta1)
044500050517     c                   Add       1             curline
044600050517     c                   ExSr      Sr_Over
044700050518     c                   If        wover = *On
044800050518     c                   Write     dn91d19
044900050518     c                   EndIf
045000050415     c                   Write     dn91d3
045100050418     c                   Clear                   desta1
045200050415      * riporto il contatore a 1
045300050415     c                   Eval      conta = 1
045400050415      * mi riposiziono sul file delle note all'ultima nota letta e torno a leggere
045500050415     c     kFndcs01      Setll     Fndcs01l
045600050415     c                   Iter
045700050415     c                   EndIf
045800050415     c                   Endsl
045900050415      * campi diversi se primo giro o giri sucessivi
046000050415     c                   If        wgiro = 1
046100050418     c                   Eval      destag = %trim(destag) + ' ' + dcsnot
046200050415     c                   Else
046300050418     c                   Eval      desta1 = %trim(desta1) + ' ' + dcsnot
046400050415     c                   EndIf
046500050415      * incremento il contatore delle note lette
046600050415     c                   Add       1             conta
046700050415     c                   EndDo
046800050415     c                   Select
046900050415      * --> STAMPO ultimo record note
047000050415     c                   When      wnote = *On
047100050517     c                   If        wgiro = 1 and conta > 1
047200050415     c                             and wnote = *On
047300050517     c                   Eval      destag = %trim(destag)
047400050517     c                   Add       1             curline
047500050517     c                   ExSr      Sr_Over
047600050518     c                   If        wover = *On
047700050518     c                   Write     dn91d19
047800050518     c                   EndIf
047900050415     c                   Write     dn91d2
048000050415     c                   EndIf
048100050517     c                   If        wgiro > 1 and conta > 1
048200050415     c                             and wnote = *On
048300050517     c                   Eval      desta1 = %trim(desta1)
048400050517     c                   Add       1             curline
048500050517     c                   ExSr      Sr_Over
048600050518     c                   If        wover = *On
048700050518     c                   Write     dn91d19
048800050518     c                   EndIf
048900050418     c                   Write     dn91d3
049000050415     c                   EndIf
049100050415      * --> STAMPO 2° record (la riga dell'anomalia) se non ho trovato descrizioni aggiuntive
049200050415     c                   other
049300050517     c                   Add       1             curline
049400050415     c                   Write     dn91d2
049500050415     c                   EndSl
049600050428
049700050429      *
    Motivo del danno                   ?
049800050428     c                   Eval      wnote = *Off
049900050428      * cerco le descrizioni aggiuntive
050000050428     c                   Eval      kdcstpd = 'C'
050100050428     c                   Movel(p)  dsnumca       kdcsnkt
050200050428     c                   Eval      kdcsstd = 'M'
050300050428     c                   Clear                   kdcsdim
050400050428     c                   Clear                   kdcshim
050500050428     c                   Clear                   kdcsnks
050600050428     c                   Eval      kdcstrc = 'D'
050700050428     c                   Eval      conta = 1
050800050428     c                   Eval      wgiro = 1
050900050428     c     kFndcs        Setll     Fndcs01l
051000050428     c                   Do        *Hival
051100050428     c     kFndcs        Reade     Fndcs01l
051200050428     c                   If        %Eof(Fndcs01l)
051300050428     c                   Leave
051400050428     c                   EndIf
051500050428     c                   Eval      wnote = *On
051600050428     c                   Select
051700050428      * se primo giro devo stampare 4 note assieme
051800050428     c                   When      wgiro = 1
051900050428      * se ho già caricato 4 note salvo la chiave della nota che sto leggendo x poi riposizionarmi
052000050428     c                   If        conta = 5
052100050428     c                   Eval      kdcstpd = dcstpd
052200050428     c                   Eval      kdcsnkt = dcsnkt
052300050428     c                   Eval      kdcsstd = dcsstd
052400050428     c                   Eval      kdcsdim = dcsdim
052500050428     c                   Eval      kdcshim = dcshim
052600050428     c                   Eval      kdcsnks = dcsnks
052700050428     c                   Eval      kdcstrc = dcstrc
052800050428     c                   Eval      kdcspno = dcspno
052900050428      * --> STAMPO 4° record
053000050517     c                   Eval      desmtv = %trim(desmtv)
053100050517     c                   Add       1             curline
053200050517     c                   ExSr      Sr_Over
053300050518     c                   If        wover = *On
053400050518     c                   Write     dn91d19
053500050518     c                   EndIf
053600050428     c                   Write     dn91d4
053700050428      * riporto il contatore a 1
053800050428     c                   Eval      conta = 1
053900050428     c                   Eval      wgiro = 2
054000050428      * mi riposiziono sul file delle note all'ultima nota letta e torno a leggere
054100050428     c     kFndcs01      Setll     Fndcs01l
054200050428     c                   Iter
054300050428     c                   EndIf
054400050518      * se non è il primo giro devo stampare 5 note assieme
054500050428     c                   Other
054600050518      * se ho già caricato 5 note salvo la chiave della nota che sto leggendo x poi riposizionarmi
054700050518     c                   If        conta = 6
054800050428     c                   Eval      kdcstpd = dcstpd
054900050428     c                   Eval      kdcsnkt = dcsnkt
055000050428     c                   Eval      kdcsstd = dcsstd
055100050428     c                   Eval      kdcsdim = dcsdim
055200050428     c                   Eval      kdcshim = dcshim
055300050428     c                   Eval      kdcsnks = dcsnks
055400050428     c                   Eval      kdcstrc = dcstrc
055500050428     c                   Eval      kdcspno = dcspno
055600050428      * --> STAMPO 5° record
055700050517     c                   Eval      desmtv1 = %trim(desmtv1)
055800050517     c                   Add       1             curline
055900050517     c                   ExSr      Sr_Over
056000050518     c                   If        wover = *On
056100050518     c                   Write     dn91d19
056200050518     c                   EndIf
056300050428     c                   Write     dn91d5
056400050428     c                   Clear                   desmtv1
056500050428      * riporto il contatore a 1
056600050428     c                   Eval      conta = 1
056700050428      * mi riposiziono sul file delle note all'ultima nota letta e torno a leggere
056800050428     c     kFndcs01      Setll     Fndcs01l
056900050428     c                   Iter
057000050428     c                   EndIf
057100050428     c                   Endsl
057200050428      * campi diversi se primo giro o giri sucessivi
057300050428     c                   If        wgiro = 1
057400050428     c                   Eval      desmtv = %trim(desmtv) + ' ' + dcsnot
057500050428     c                   Else
057600050428     c                   Eval      desmtv1 = %trim(desmtv1) + ' ' + dcsnot
057700050428     c                   EndIf
057800050428      * incremento il contatore delle note lette
057900050428     c                   Add       1             conta
058000050428     c                   EndDo
058100050428     c                   Select
058200050428      * --> STAMPO ultimo record note
058300050428     c                   When      wnote = *On
058400050517     c                   If        wgiro = 1 and conta > 1
058500050428     c                             and wnote = *On
058600050517     c                   Eval      desmtv = %trim(desmtv)
058700050517     c                   Add       1             curline
058800050517     c                   ExSr      Sr_Over
058900050518     c                   If        wover = *On
059000050518     c                   Write     dn91d19
059100050518     c                   EndIf
059200050428     c                   Write     dn91d4
059300050428     c                   EndIf
059400050517     c                   If        wgiro > 1 and conta > 1
059500050428     c                             and wnote = *On
059600050517     c                   Eval      desmtv1 = %trim(desmtv1)
059700050517     c                   Add       1             curline
059800050517     c                   ExSr      Sr_Over
059900050518     c                   If        wover = *On
060000050518     c                   Write     dn91d19
060100050518     c                   EndIf
060200050428     c                   Write     dn91d5
060300050428     c                   EndIf
060400050428     c                   EndSl
060500050415
060600050429      *
    Pezzi presenti                     ?
060700050429     c                   Eval      wnote = *Off
060800050429      * cerco le descrizioni aggiuntive
060900050429     c                   Eval      kdcstpd = 'C'
061000050429     c                   Movel(p)  dsnumca       kdcsnkt
061100050429     c                   Eval      kdcsstd = 'K'
061200050429     c                   Clear                   kdcsdim
061300050429     c                   Clear                   kdcshim
061400050429     c                   Clear                   kdcsnks
061500050429     c                   Eval      kdcstrc = 'D'
061600050429     c                   Eval      conta = 1
061700050429     c                   Eval      wgiro = 1
061800050429     c     kFndcs        Setll     Fndcs01l
061900050429     c                   Do        *Hival
062000050429     c     kFndcs        Reade     Fndcs01l
062100050429     c                   If        %Eof(Fndcs01l)
062200050429     c                   Leave
062300050429     c                   EndIf
062400050429     c                   Eval      wnote = *On
062500050429     c                   Select
062600050429      * se primo giro devo stampare 4 note assieme
062700050429     c                   When      wgiro = 1
062800050429      * se ho già caricato 4 note salvo la chiave della nota che sto leggendo x poi riposizionarmi
062900050429     c                   If        conta = 5
063000050429     c                   Eval      kdcstpd = dcstpd
063100050429     c                   Eval      kdcsnkt = dcsnkt
063200050429     c                   Eval      kdcsstd = dcsstd
063300050429     c                   Eval      kdcsdim = dcsdim
063400050429     c                   Eval      kdcshim = dcshim
063500050429     c                   Eval      kdcsnks = dcsnks
063600050429     c                   Eval      kdcstrc = dcstrc
063700050429     c                   Eval      kdcspno = dcspno
063800050429      * --> STAMPO 6° record
063900050517     c                   Eval      desprs = %trim(desprs)
064000050517     c                   Add       1             curline
064100050517     c                   ExSr      Sr_Over
064200050518     c                   If        wover = *On
064300050518     c                   Write     dn91d19
064400050518     c                   EndIf
064500050429     c                   Write     dn91d6
064600050429      * riporto il contatore a 1
064700050429     c                   Eval      conta = 1
064800050429     c                   Eval      wgiro = 2
064900050429      * mi riposiziono sul file delle note all'ultima nota letta e torno a leggere
065000050429     c     kFndcs01      Setll     Fndcs01l
065100050429     c                   Iter
065200050429     c                   EndIf
065300050518      * se non è il primo giro devo stampare 5 note assieme
065400050429     c                   Other
065500050518      * se ho già caricato 5 note salvo la chiave della nota che sto leggendo x poi riposizionarmi
065600050518     c                   If        conta = 6
065700050429     c                   Eval      kdcstpd = dcstpd
065800050429     c                   Eval      kdcsnkt = dcsnkt
065900050429     c                   Eval      kdcsstd = dcsstd
066000050429     c                   Eval      kdcsdim = dcsdim
066100050429     c                   Eval      kdcshim = dcshim
066200050429     c                   Eval      kdcsnks = dcsnks
066300050429     c                   Eval      kdcstrc = dcstrc
066400050429     c                   Eval      kdcspno = dcspno
066500050429      * --> STAMPO 7° record
066600050517     c                   Eval      desprs1 = %trim(desprs1)
066700050517     c                   Add       1             curline
066800050517     c                   ExSr      Sr_Over
066900050518     c                   If        wover = *On
067000050518     c                   Write     dn91d19
067100050518     c                   EndIf
067200050429     c                   Write     dn91d7
067300050429     c                   Clear                   desprs1
067400050429      * riporto il contatore a 1
067500050429     c                   Eval      conta = 1
067600050429      * mi riposiziono sul file delle note all'ultima nota letta e torno a leggere
067700050429     c     kFndcs01      Setll     Fndcs01l
067800050429     c                   Iter
067900050429     c                   EndIf
068000050429     c                   Endsl
068100050429      * campi diversi se primo giro o giri sucessivi
068200050429     c                   If        wgiro = 1
068300050429     c                   Eval      desprs = %trim(desprs) + ' ' + dcsnot
068400050429     c                   Else
068500050429     c                   Eval      desprs1 = %trim(desprs1) + ' ' + dcsnot
068600050429     c                   EndIf
068700050429      * incremento il contatore delle note lette
068800050429     c                   Add       1             conta
068900050429     c                   EndDo
069000050429     c                   Select
069100050429      * --> STAMPO ultimo record note
069200050429     c                   When      wnote = *On
069300050517     c                   If        wgiro = 1 and conta > 1
069400050429     c                             and wnote = *On
069500050517     c                   Eval      desprs = %trim(desprs)
069600050517     c                   Add       1             curline
069700050517     c                   ExSr      Sr_Over
069800050518     c                   If        wover = *On
069900050518     c                   Write     dn91d19
070000050518     c                   EndIf
070100050429     c                   Write     dn91d6
070200050429     c                   EndIf
070300050517     c                   If        wgiro > 1 and conta > 1
070400050429     c                             and wnote = *On
070500050517     c                   Eval      desprs1 = %trim(desprs1)
070600050517     c                   Add       1             curline
070700050517     c                   ExSr      Sr_Over
070800050518     c                   If        wover = *On
070900050518     c                   Write     dn91d19
071000050518     c                   EndIf
071100050429     c                   Write     dn91d7
071200050429     c                   EndIf
071300050429     c                   EndSl
071400050429
071500050429      *
    Pezzi mancanti                     ?
071600050429     c                   Eval      wnote = *Off
071700050429      * cerco le descrizioni aggiuntive
071800050429     c                   Eval      kdcstpd = 'C'
071900050429     c                   Movel(p)  dsnumca       kdcsnkt
072000050429     c                   Eval      kdcsstd = 'P'
072100050429     c                   Clear                   kdcsdim
072200050429     c                   Clear                   kdcshim
072300050429     c                   Clear                   kdcsnks
072400050429     c                   Eval      kdcstrc = 'D'
072500050429     c                   Eval      conta = 1
072600050429     c                   Eval      wgiro = 1
072700050429     c     kFndcs        Setll     Fndcs01l
072800050429     c                   Do        *Hival
072900050429     c     kFndcs        Reade     Fndcs01l
073000050429     c                   If        %Eof(Fndcs01l)
073100050429     c                   Leave
073200050429     c                   EndIf
073300050429     c                   Eval      wnote = *On
073400050429     c                   Select
073500050429      * se primo giro devo stampare 4 note assieme
073600050429     c                   When      wgiro = 1
073700050429      * se ho già caricato 4 note salvo la chiave della nota che sto leggendo x poi riposizionarmi
073800050429     c                   If        conta = 5
073900050429     c                   Eval      kdcstpd = dcstpd
074000050429     c                   Eval      kdcsnkt = dcsnkt
074100050429     c                   Eval      kdcsstd = dcsstd
074200050429     c                   Eval      kdcsdim = dcsdim
074300050429     c                   Eval      kdcshim = dcshim
074400050429     c                   Eval      kdcsnks = dcsnks
074500050429     c                   Eval      kdcstrc = dcstrc
074600050429     c                   Eval      kdcspno = dcspno
074700050429      * --> STAMPO 8° record
074800050517     c                   Eval      desnpz = %trim(desnpz)
074900050517     c                   Add       1             curline
075000050517     c                   ExSr      Sr_Over
075100050518     c                   If        wover = *On
075200050518     c                   Write     dn91d19
075300050518     c                   EndIf
075400050429     c                   Write     dn91d8
075500050429      * riporto il contatore a 1
075600050429     c                   Eval      conta = 1
075700050429     c                   Eval      wgiro = 2
075800050429      * mi riposiziono sul file delle note all'ultima nota letta e torno a leggere
075900050429     c     kFndcs01      Setll     Fndcs01l
076000050429     c                   Iter
076100050429     c                   EndIf
076200050518      * se non è il primo giro devo stampare 5 note assieme
076300050429     c                   Other
076400050518      * se ho già caricato 5 note salvo la chiave della nota che sto leggendo x poi riposizionarmi
076500050518     c                   If        conta = 6
076600050429     c                   Eval      kdcstpd = dcstpd
076700050429     c                   Eval      kdcsnkt = dcsnkt
076800050429     c                   Eval      kdcsstd = dcsstd
076900050429     c                   Eval      kdcsdim = dcsdim
077000050429     c                   Eval      kdcshim = dcshim
077100050429     c                   Eval      kdcsnks = dcsnks
077200050429     c                   Eval      kdcstrc = dcstrc
077300050429     c                   Eval      kdcspno = dcspno
077400050429      * --> STAMPO 9° record
077500050517     c                   Eval      desnpz1 = %trim(desnpz1)
077600050517     c                   Add       1             curline
077700050517     c                   ExSr      Sr_Over
077800050518     c                   If        wover = *On
077900050518     c                   Write     dn91d19
078000050518     c                   EndIf
078100050429     c                   Write     dn91d9
078200050429     c                   Clear                   desnpz1
078300050429      * riporto il contatore a 1
078400050429     c                   Eval      conta = 1
078500050429      * mi riposiziono sul file delle note all'ultima nota letta e torno a leggere
078600050429     c     kFndcs01      Setll     Fndcs01l
078700050429     c                   Iter
078800050429     c                   EndIf
078900050517     c                   EndSl
079000050429      * campi diversi se primo giro o giri sucessivi
079100050429     c                   If        wgiro = 1
079200050429     c                   Eval      desnpz = %trim(desnpz) + ' ' + dcsnot
079300050429     c                   Else
079400050429     c                   Eval      desnpz1 = %trim(desnpz1) + ' ' + dcsnot
079500050429     c                   EndIf
079600050429      * incremento il contatore delle note lette
079700050429     c                   Add       1             conta
079800050429     c                   EndDo
079900050429     c                   Select
080000050429      * --> STAMPO ultimo record note
080100050429     c                   When      wnote = *On
080200050517     c                   If        wgiro = 1 and conta > 1
080300050429     c                             and wnote = *On
080400050517     c                   Eval      desnpz = %trim(desnpz)
080500050517     c                   Add       1             curline
080600050517     c                   ExSr      Sr_Over
080700050518     c                   If        wover = *On
080800050518     c                   Write     dn91d19
080900050518     c                   EndIf
081000050429     c                   Write     dn91d8
081100050429     c                   EndIf
081200050517     c                   If        wgiro > 1 and conta > 1
081300050429     c                             and wnote = *On
081400050517     c                   Eval      desnpz1 = %trim(desnpz1)
081500050517     c                   Add       1             curline
081600050517     c                   ExSr      Sr_Over
081700050518     c                   If        wover = *On
081800050518     c                   Write     dn91d19
081900050518     c                   EndIf
082000050429     c                   Write     dn91d9
082100050429     c                   EndIf
082200050429     c                   EndSl
082300050429
082400050429      *
    N.pezzi mancanti/colli/Kg          ?
082500050429     c                   Eval      pdctnpz = dctnpz
082600050429     c                   Eval      pdctncn = dctncn
082700050429     c                   Eval      pdctpkd = dctpkd
082800050429
082900050429      *
    Verifica DDT                       ?
083000050415     c                   If        dcttdt = 'P'
083100050415     c                   Eval      destdt = 'PARTENZA'
083200050415     c                   EndIf
083300050415     c                   If        dcttdt = 'A'
083400050415     c                   Eval      destdt = 'ARRIVO'
083500050415     c                   EndIf
083600050415     c                   If        dcttdt = *Blanks
083700050415     c                   Eval      destdt = 'NESSUNO'
083800050415     c                   EndIf
083900050429
084000050429      *
    Riserva                            ?
084100050415     c                   If        dctcrc  = 'S'
084200050415     c                   Eval      descrc  = 'SI'
084300050415     c                   EndIf
084400050415     c                   If        dctcrc  <> 'S'
084500050415     c                   Eval      descrc  = 'NO'
084600050415     c                   EndIf
084700050429
084800050429      *
    Disposizioni sulle merce danneg.   ?
084900050415     c                   If        dctdsd  = 'C'
085000050415     c                   Eval      desdsd = 'CONSEGNATA'
085100050415     c                   EndIf
085200050415     c                   If        dctdsd  = 'D'
085300050415     c                   Eval      desdsd = 'DISTRUTTA'
085400050415     c                   EndIf
085500050415     c                   If        dctdsd  = 'R'
085600050415     c                   Eval      desdsd = 'RESA'
085700050415     c                   EndIf
085800050415     c                   If        dctdsd  = 'M'
085900050415     c                   Eval      desdsd = 'MAGAZZINO'
086000050415     c                   EndIf
086100050412
086200050429      *
    Beneficiario alternativo           ?
086300050412     c                   If        dctksc > *Zeros
086400050415     c                   Eval      pdctksc = dctksc
086500050412     c                   Move      dctksc        w0040
086600050412      *   se codificato decodifico da p.d.c.
086700050412     c                   If        w0040 <> 8888
086800050412     c                   Clear                   tibs69ds
086900050412     c                   z-add     dctksc        I69kac
087000050412     c                   Call      'TIBS69R'
087100050412     c                   Parm                    tibs69DS
087200050412     c                   Parm                    ds_cnaco
087300050412     c                   Parm                    ds_cnind
087400050412     c                   Parm                    ds_cnclp
087500050412     c                   Parm                    ds_fncls
087600050429     c                   If        o69err <> '1'
087700050412     c                   Eval      desksc = acorag
087800050412     c                   EndIf
087900050412     c                   Else
088000050412      *   se non codificato decodifico da anagrafica clienti c.a.
088100050412     c                   Eval      wtrc = 'C'
088200050412     c     kFndka        Chain     Fndka01l
088300050414     c                   If        %Found(Fndka01l) and dkaatb <> 'A'
088400050412     c                   Eval      desksc = dkarag
088500050412     c                   EndIf
088600050412     c                   EndIf
088700050412     c                   EndIf
088800050415
088900050429      *
    Interlocutore                      ?
089000050415     c                   Eval      kdcstpd = 'C'
089100050415     c                   Movel(p)  dsnumca       kdcsnkt
089200050415     c                   Eval      kdcsstd = 'N'
089300050415     c                   Clear                   kdcsdim
089400050415     c                   Clear                   kdcshim
089500050415     c                   Clear                   kdcsnks
089600050415     c                   Eval      kdcstrc = 'D'
089700050415     c     kFndcs        Chain     Fndcs01l
089800050415     c                   If        %Found(Fndcs01l)
089900050415     c                   Eval      desint = dcsnot
090000050415     c                   EndIf
090100050415
090200050429      *
    Responsabilità internazionale      ?
090300050415     c                   Eval      *In02 = *Off
090400050415     c                   If        dctres = 'V'
090500050415     c                   Eval      desres = 'VETTORE'
090600050415     c                   Eval      *In02 = *On
090700050415     c                   EndIf
090800050415     c                   If        dctres = 'P'
090900050415     c                   Eval      desres = 'PARTNER'
091000050415     c                   Eval      *In02 = *On
091100050415     c                   EndIf
091200050414
091300050429      *
    Rivalsa                            ?
091400050414     c                   Eval      *In01 = *Off
091500050414     c                   Eval      wtrc = 'R'
091600050414     c     kFndka        Chain     Fndka01l
091700050414     c                   If        %Found(Fndka01l) and dkaatb <> 'A'
091800050414     c                   Eval      *In01 = *On
091900050414     c                   Eval      desass = dkarag
092000050414     c                   Eval      codass = dkaksc
092100050414     c                   Eval      viaass = dkavia
092200050414     c                   Eval      capass = dkacap
092300050414     c                   Eval      locass = dkaloc
092400050414     c                   Eval      prvass = dkaprv
092500050414     c                   Eval      nazass = dkanaz
092600050414     c                   Eval      telass = dkatel
092700050414     c                   Eval      faxass = dkafax
092800050414     c                   EndIf
092900050429
093000050429      *
    Causale chiusura C.A.              ?
093100050429     c                   Eval      pdctcch = dctcch
093200050429     c                   Clear                   dcch
093300050429     c                   Clear                   tibs02ds
093400050429     c                   Eval      t02mod = 'C'
093500050429     c                   Eval      t02sif = knsif
093600050429     c                   Eval      t02cod = 'CCH'
093700050429     c                   Eval      t02ke1 = dctcch
093800050429     c                   Call      'TIBS02R'
093900050429     c                   Parm                    kpjba
094000050429     c                   Parm                    tibs02ds
094100050429     c                   If        t02err = *Blanks
094200050429     c                   Eval      dcch = t02uni
094300050429     c                   EndIf
094400050429     c                   Eval      descch = §cchdesc
094500050429
094600050429      *
    Pagamento                          ?
094700050429      * controllo se è stata fatta almeno un'estrazione o il pagamento
094800050429     c                   If        §dctpaga = 'C'
094900050429     c                   Eval      despag = 'Estratta per il pagamento'
095000050429     c                   EndIf
095100050429     c                   If        §dctpaga = 'P'
095200050429     c                   Eval      despag = 'Pagata'
095300050429     c                   EndIf
095400050429
095500050429      *
    Mandato assicurativo               ?
095600050503     c                   Eval      *In05 = *Off
095700050429     c     kFndc         Chain     Fndpt01l
095800050429     c                   If        %Found(Fndpt01l) and dptatb <> 'A'
095900050429     c                   Eval      *In05 = *On
096000050429     c                   Eval      pdptksc = dptksc
096100050429     c                   If        dptftc = *Blanks
096200050429     c                   Eval      desman = 'CLIENTE SENZA MANDATO'
096300050429     c                   Else
096400050429     c                   Clear                   ds1p
096500050429     c                   Eval      ktblcod = '1P'
096600050429     c                   Eval      ktblkey = dptftc
096700050429     c     kTab          Chain     Tabel00f
096800050429     c                   If        %Found(tabel00f)
096900050429     c                   Eval      ds1p = tbluni
097000050429     c                   EndIf
097100050429     c                   Eval      desman = §1pdes
097200050429     c                   EndIf
097300050429      * Ragione sociale cliente
097400050429     c                   Clear                   tibs69ds
097500050429     c                   z-add     dptksc        I69kac
097600050429     c                   Call      'TIBS69R'
097700050429     c                   Parm                    tibs69DS
097800050429     c                   Parm                    ds_cnaco
097900050429     c                   Parm                    ds_cnind
098000050429     c                   Parm                    ds_cnclp
098100050429     c                   Parm                    ds_fncls
098200050429     c                   If        o69err <> '1'
098300050429     c                   Eval      ragdpt = acorag
098400050429     c                   EndIf
098500050429      * codice tariffa
098600050429     c                   Eval      pdptctr = dptctr
098700050429     c                   Clear                   dstr
098800050429     c                   Eval      ktblcod = 'TR'
098900050429     c                   Movel     dptctr        w001a
099000050429     c                   Eval      ktblkey = w001a
099100050429     c     kTab          Chain     Tabel00f
099200050429     c                   If        %Found(tabel00f)
099300050429     c                   Eval      dstr = tbluni
099400050429     c                   EndIf
099500050429     c                   Eval      desctr = §trdes
099600050429      * tipo valore merce
099700050429     c                   Eval      pdptvlm = dptvlm
099800050429     c                   Eval      pdptvvm = dptvvm
099900050429     c                   Clear                   dstr
100000050429     c                   Eval      ktblcod = 'TR'
100100050429     c                   Eval      ktblkey = dptfvm
100200050429     c     kTab          Chain     Tabel00f
100300050429     c                   If        %Found(tabel00f)
100400050429     c                   Eval      dstr = tbluni
100500050429     c                   EndIf
100600050429     c                   Eval      desvvm = §trde3
100700050429     c                   EndIf
100800050429
100900050429      * --> STAMPO 10° record
101000050517     c                   Add       3             curline
101100050517     c   01              Add       1             curline
101200050517     c   05              Add       1             curline
101300050517     c                   ExSr      Sr_Over
101400050518     c                   If        wover = *On
101500050518     c                   Write     dn91d19
101600050518     c                   EndIf
101700050429     c                   Write     dn91d10
101800050415
101900050429      *
    Importo da assicurare              ?
102000050415     c                   Eval      ptasias = tasias
102100050415     c                   Eval      ptasvas = tasvas
102200050415      * se inserito da fatturazione non stampo l'importo da assicurare
102300050415     c                   If        tasfcm = 'F'
102400050415     c                   Eval      *In03 = *Off
102500050415     c                   Else
102600050415     c                   Eval      *In03 = *On
102700050415     c                   EndIf
102800050415
102900050429      *
    Importo Max risarcibile            ?
103000050415     c                   Eval      pdctipv = dctipv
103100050415     c                   Eval      pdctvpv = dctvpv
103200050415      * Tipo inserimento importo max risarcibile
103300050413     c                   If        dctfpv = 'M'
103400050413     c                   Eval      desfpv = 'INSERITO MANUALMENTE'
103500050413     c                   EndIf
103600050413     c                   If        dctfpv = 'C'
103700050413     c                   Eval      desfpv = 'CALCOLATO'
103800050413     c                   EndIf
103900050415
104000050415      * Aggancio file Liquidazione C.A.
104100050415     c     kFndc         Chain     Fndcl01l
104200050415     c                   If        not %Found(Fndcl01l)
104300050418     c                   ExSr      Puldcl
104400050415     c                   EndIf
104500050415
104600050429      *
    Importo richiesto                  ?
104700050415     c                   Eval      pdclipr = dclipr
104800050415     c                   Eval      pdclvpr = dclvpr
104900050413
105000050429      *
    Importo trattenuto                 ?
105100050415     c                   Eval      pdclipt = dclipt
105200050415     c                   Eval      pdclvpt = dclvpt
105300050415      * data importo trattenuto
105400050414     c                   If        dcldpt > *Zeros
105500050414     c     *iso          Move      dcldpt        wdataeur
105600050414     c     *eur          Movel     wdataeur      datapt
105700050414     c                   EndIf
105800050415
105900050429      *
    Importo liquidato                  ?
106000050418      * se è pratica è in un campo... se è liquidazione in un altro
106100050418     c                   If        *In20
106200050418     c                   Eval      pdclipl = dclipl
106300050418     c                   Eval      pdclvpl = dclvpl
106400050418     c                   Else
106500050418     c                   If        *In21
106600050418     c                   Eval      impipp = dclipl
106700050418     c                   Eval      valipp = dclvpl
106800050418     c                   EndIf
106900050418      * se pratica trattenuta da cliente e pagamento già avvenuto imposto come importo
107000050418      * liquidato la differenza
107100050418     c                   If        dclipl <= dclipt
107200050418     c                   Clear                   pdclipl
107300050418     c                   Clear                   pdclvpl
107400050418     c                   Else
107500050418     c                   Eval      pdclipl = dclipl -dclipt
107600050418     c                   Eval      pdclvpl = dclvpl
107700050418     c                   EndIf
107800050418     c                   EndIf
107900050415     c
108000050414
108100050429      *
    Data emissione assegno             ?
108200050414     c                   If        dcldea > *Zeros
108300050414     c     *iso          Move      dcldea        wdataeur
108400050414     c     *eur          Movel     wdataeur      dataea
108500050414     c                   EndIf
108600050414
108700050429      *
    Decodifica ABI/CAB                 ?
108800050414     c                   Move      dclabi        wabi
108900050414     c                   Move      dclcab        wcab
109000050414     c     kCnabi        Chain     Cnabi01l
109100050414     c                   If        %Found (Cnabi01l)
109200050414     c                   Eval      desban = %trim(abiist) + '-' + %trim(abiage)
109300050414     c                             + '-' + %trim(abiloc) + '-' + %trim(abicom)
109400050414     c                   EndIf
109500050418
109600050429      * --> STAMPO 11° record
109700050517     c                   Add       2             curline
109800050517     c                   ExSr      Sr_Over
109900050518     c                   If        wover = *On
110000050518     c                   Write     dn91d19
110100050518     c                   EndIf
110200050429     c                   Write     dn91d11
110300050414
110400050429      *
    Intestatario progetto di liquidaz. ?
110500050418      * Solo se è Pratica Assicurativa o Liquidazione Transattiva
110600050418     c                   If        *In20 or *In21
110700050414     c                   Eval      wtrc = 'I'
110800050414     c     kFndka        Chain     Fndka01l
110900050414     c                   If        %Found(Fndka01l) and dkaatb <> 'A'
111000050414     c                   Eval      despli = dkarag
111100050414     c                   Eval      codpli = dkaksc
111200050414     c                   Eval      viapli = dkavia
111300050429     c                   Eval      locpli = %trim(dkacap) + ' ' + %trim(dkaloc)
111400050429     c                                    + '  ' + dkaprv + ' ' + dkanaz
111500050414     c                   EndIf
111600050414
111700050429      *
    Data stp. progetto di liquidazione ?
111800050414     c                   If        dcldds > *Zeros
111900050414     c     *iso          Move      dcldds        wdataeur
112000050414     c     *eur          Movel     wdataeur      datads
112100050414     c                   EndIf
112200050429
112300050429      *
    Data accett. prg. di liquidazione  ?
112400050414      * controllo la validità della data xchè ce ne sono delle farlocche soprattutto
112500050414      * nella vecchie c.a.
112600050504      * se errata metto data valuta pagamento
112700050414     c                   If        dcldpl > *Zeros
112800050414     c                   clear                   wlbdat
112900050419     c                   Z-add     dcldpl        g02inv
113000050419     c                   Eval      g02err = '3'
113100050414     c                   call      'XSRDA8'
113200050414     c                   parm                    wlbdat
113300050419     c                   If        g02err = '1'
113400050414     c     *iso          Move      dclvlp        wdataeur
113500050414     c     *eur          Movel     wdataeur      datapl
113600050414     c                   Else
113700050510     c                   Move      g02dat        datapl
113800050414     c                   EndIf
113900050414     c                   EndIf
114000050414
114100050429      *
    Data valuta pagamento              ?
114200050504      * controllo la validità della data xchè ce ne sono delle farlocche soprattutto
114300050504      * nella vecchie c.a.
114400050504      * se errata metto data accett. prg. di liquidazione
114500050414     c                   If        dclvlp > *Zeros
114600050504     c                   clear                   wlbdat
114700050510     c                   Z-add     dclvlp        g02inv
114800050504     c                   Eval      g02err = '3'
114900050504     c                   call      'XSRDA8'
115000050504     c                   parm                    wlbdat
115100050504     c                   If        g02err = '1'
115200050504     c     *iso          Move      dcldpl        wdataeur
115300050504     c     *eur          Movel     wdataeur      datalp
115400050504     c                   Else
115500050510     c                   Move      g02dat        datalp
115600050414     c                   EndIf
115700050504     c                   EndIf
115800050418     c                   EndIf
115900050429
116000050429      *
    Note calcolo progetto di liq.      ?
116100050429      *     solo 4 note
116200050503     c                   Eval      *In06 = *Off
116300050429     c                   If        dcldds > *Zeros
116400050429     c                   Eval      kdcstpd = 'C'
116500050429     c                   Movel(p)  dsnumca       kdcsnkt
116600050429     c                   Eval      kdcsstd = 'C'
116700050429     c                   Clear                   kdcsdim
116800050429     c                   Clear                   kdcshim
116900050429     c                   Clear                   kdcsnks
117000050429     c                   Eval      kdcstrc = 'D'
117100050429     c     kFndcs        Setll     Fndcs01l
117200050429     c                   Do        *Hival
117300050429     c     kFndcs        Reade     Fndcs01l
117400050429     c                   If        %Eof(Fndcs01l)
117500050429     c                   Leave
117600050429     c                   EndIf
117700050429     c                   Eval      desplt = %trim(desplt) + ' ' + dcsnot
117800050429     c                   Eval      *In06 = *On
117900050429     c                   EndDo
118000050429     c                   EndIf
118100050418
118200050429      * --> STAMPO 12° record
118300050517     c                   Add       2             curline
118400050517     c   06              Add       1             curline
118500050517     c                   ExSr      Sr_Over
118600050518     c                   If        wover = *On
118700050518     c                   Write     dn91d19
118800050518     c                   EndIf
118900050429     c                   Write     dn91d12
119000070104
119100070104      *
    RECUPERO DA A.I.G.                 ?
119200090414     c*****              clear                   datarr
119300090414     c*****              clear                   datavr
119400090414     c*****              clear                   datard
119500070104      * controllo se esite la C.A. sul file dei recuperi A.I.G.
119600090414     c*****kfndc         chain     fndra01l
119700090414     c*****              if        %found (fndra01l) and draatb = ' '
119800070104      * verifico se CA esclusa dal rimborso
119900090414     c*****              if          draesc = 'E'
120000070104      * --> STAMPO 22° record
120100090414     c*****              add       1             curline
120200090414     c*****              exsr      sr_over
120300090414     c*****              if        wover = *on
120400090414     c*****              write     dn91d19
120500090414     c*****              endif
120600090414     c*****              write     dn91d22
120700070104
120800070104      * se non è esclusa stampo i dati del recupero
120900090414     c*****              else
121000070104      * data richiesta recupero
121100090414     c*****              if        dradrr > *zeros
121200090414     c******iso          move      dradrr        wdataeur
121300090414     c******eur          movel     wdataeur      datarr
121400090414     c*****              endif
121500070104      * data ricezione recupero
121600090414     c*****              if        dradvr > *zeros
121700090414     c******iso          move      dradvr        wdataeur
121800090414     c******eur          movel     wdataeur      datavr
121900090414     c*****              endif
122000070104      * --> STAMPO 20° record
122100090414     c*****              add       1             curline
122200090414     c*****              exsr      sr_over
122300090414     c*****              if        wover = *on
122400090414     c*****              write     dn91d19
122500090414     c*****              endif
122600090414     c*****              write     dn91d20
122700070104
122800070104      * cerco anche le disposizioni A.I.G.
122900090414     c*****              eval      kdcstpd = 'C'
123000090414     c*****              movel(p)  dsnumca       kdcsnkt
123100090414     c*****              eval      kdcsstd = 'D'
123200090414     c*****              clear                   kdcsdim
123300090414     c*****              clear                   kdcshim
123400090414     c*****              clear                   kdcsnks
123500090414     c*****              eval      kdcstrc = 'D'
123600090414     c*****              eval      conta = 1
123700090414     c*****              eval      wgiro = 1
123800090414     c*****kfndcs        setll     fndcs01l
123900090414     c*****              do        *hival
124000090414     c*****kfndcs        reade     fndcs01l
124100090414     c*****              if        %eof(fndcs01l)
124200090414     c*****              leave
124300090414     c*****              endif
124400090414     c*****              if        dcsatb = 'A'
124500090414     c*****              iter
124600090414     c*****              endif
124700090414     c*****              eval      *in08 = *off
124800070104      * se ho già caricato 3 note nel campo nota
124900070104      * stampo
125000090414     c*****              if        conta = 4
125100070104      * se non è la prima volta che stampo non stampo la data
125200090414     c*****              if        wgiro > 1
125300090414     c*****              eval      *in08 = *on
125400090414     c*****              endif
125500070104      * se è la prima volta che stampo devo stampare la data
125600090414     c*****              if        wgiro = 1
125700090414     c*****              add       1             wgiro
125800090414     c*****              endif
125900090414     c*****              eval      rigadisp = %trim(rigadisp)
126000070104      * --> STAMPO 21° record
126100090414     c*****              add       1             curline
126200090414     c*****              exsr      sr_over
126300090414     c*****              if        wover = *on
126400090414     c*****              write     dn91d19
126500090414     c*****              endif
126600090414     c*****              write     dn91d21
126700090414     c*****              clear                   rigadisp
126800090414     c*****              eval      conta = 1
126900090414     c*****              endif
127000070104      * data ricezione disposizioni
127100090414     c*****              if        dradrd > *zeros
127200090414     c******iso          move      dradrd        wdataeur
127300090414     c******eur          movel     wdataeur      datard
127400090414     c*****              endif
127500070104      * imposto la riga della nota 3 note x riga
127600090414     c*****              eval      rigadisp = %trim(rigadisp) + ' ' + dcsnot
127700090414     c*****              add       1             conta
127800090414     c*****              enddo
127900070104      * se a fine lettura ho ancora delle note da stampare le stampo
128000090414     c*****              if        conta > 1
128100090414     c*****              if        wgiro > 1
128200090414     c*****              eval      *in08 = *on
128300090414     c*****              endif
128400090414     c*****              eval      rigadisp = %trim(rigadisp)
128500090414     c*****              add       1             curline
128600090414     c*****              exsr      sr_over
128700090414     c*****              if        wover = *on
128800090414     c*****              write     dn91d19
128900090414     c*****              endif
129000090414     c*****              write     dn91d21
129100090414     c*****              endif
129200070104
129300090414     c*****              endif
129400090414     c*****              endif
129500050414
129600050429      *
    F A S I                            ?
129700050503     c                   Clear                   conta
129800050504     c                   Clear                   s_rigafase
129900050503      * --> STAMPO 13° record
130000050418      *     intestazione fasi
130100050517     c                   Add       3             curline
130200050517     c                   ExSr      Sr_Over
130300050503     c                   Write     dn91d13
130400050418
130500050418      * --> Fasi eseguite
130600050414     c     kFndc         Setll     Fndcf01l
130700050414     c                   Do        *Hival
130800050414     c     kFndc         Reade     Fndcf01l
130900050414     c                   If        %Eof(Fndcf01l)
131000050414     c                   Leave
131100050414     c                   EndIf
131200050414     c                   If        dcfatb = 'A'
131300050414     c                   Iter
131400050414     c                   EndIf
131500050504
131600050504     c                   Add       1             conta
131700050503
131800050503      * Descrizione fase
131900050503     c     dcffca        Chain     Fndfa01l
132000050503     c                   If        Not %Found(Fndfa01l)
132100050503     c                   Clear                   dfades
132200050503     c                   EndIf
132300050503
132400050503     c                   Eval      ddcf01 = dcflet
132500050503
132600050503      * Data esecuzione fase
132700050503     c     *iso          Move      dcfdfc        wdataeur
132800050504     c     *eur          Movel     wdataeur      datafa
132900050504
133000050504      * Imposto i campi come devono essere stampati
133100050504      * data/ora fase/descrizione flag p.o./utente
133200050504     c                   Eval      rigafase1 = %editw(datafa:'  /  /    ') +
133300050504     c                             ' ' + %editw(dcfhfc:'  :  ') + ' ' +
133400050504     c                             %editc(dcffca:'1') + ' ' + dfades + '   ' +
133500050504     c                             §dcfpfra + '    ' + dcfptr + '    ' +
133600050504     c                             %editc(dcffev:'1') + '  ' + dcfpru
133700050504
133800050504     c                   Eval      s_rigafase(conta) = rigafase1
133900050414
134000050414     c                   EndDo
134100050503
134200050504      * Stampo le fasi su due colonne
134300050504     c     conta         div       2             righe
134400050504     c                   mvr                     resto
134500050504     c                   add       resto         righe
134600050504     c                   Clear                   col1
134700050504     c                   Clear                   col2
134800050504     c                   Do        righe         col1
134900050504     c                   Eval      col2 = col1 + righe
135000050504     c                   Clear                   dn91d14
135100050504     c                   Eval      rigafase1 = s_rigafase(col1)
135200050504     c                   Eval      rigafase2 = s_rigafase(col2)
135300050503      * --> STAMPO 14° record
135400050517     c                   Add       1             curline
135500050517     c                   ExSr      Sr_Over
135600050518     c                   If        wover = *On
135700050518     c                   Write     dn91d13
135800050518     c                   EndIf
135900050503     c                   Write     dn91d14
136000050504     c                   EndDo
136100050414
136200050503      *
    Documenti Richiesti                ?
136300050509     c                   Clear                   conta
136400050509     c                   Clear                   s_rigadoc
136500050509     c     kFndc         Setll     Fndcc01l
136600050509     c                   If        %Equal(Fndcc01l)
136700050509      * --> STAMPO 15° record
136800050509      *     intestazione documenti
136900050517     c                   Add       3             curline
137000050517     c                   ExSr      Sr_Over
137100050509     c                   Write     dn91d15
137200050509
137300050509     c                   Do        *Hival
137400050509     c     kFndc         Reade     Fndcc01l
137500050509     c                   If        %Eof(Fndcc01l)
137600050509     c                   Leave
137700050509     c                   EndIf
137800050509     c                   If        dccatb = 'A'
137900050509     c                   Iter
138000050509     c                   EndIf
138100050509
138200050509     c                   Add       1             conta
138300050509
138400050509      * Descrizione tipo documento
138500050509     c                   Clear                   dcdd
138600050509     c                   Clear                   tibs02ds
138700050509     c                   Eval      t02mod = 'C'
138800050509     c                   Eval      t02sif = knsif
138900050509     c                   Eval      t02cod = 'CDD'
139000050509     c                   Eval      t02ke1 = dcccdd
139100050509     c                   Call      'TIBS02R'
139200050509     c                   Parm                    kpjba
139300050509     c                   Parm                    tibs02ds
139400050509     c                   If        t02err = *Blanks
139500050509     c                   Eval      dcdd = t02uni
139600050509     c                   EndIf
139700050509
139800050510     c                   Clear                   datarz
139900050510     c                   Clear                   datadc
140000050509      * Data ricezione documento
140100050510      * controllo la validità della data xchè ce ne sono delle farlocche soprattutto
140200050510      * nella vecchie c.a.
140300050510      * se errata la azzero e non la stampo
140400050509     c                   If        dccdrz > *Zeros
140500050510     c                   clear                   wlbdat
140600050510     c                   Z-add     dccdrz        g02inv
140700050510     c                   Eval      g02err = '3'
140800050510     c                   call      'XSRDA8'
140900050510     c                   parm                    wlbdat
141000050510     c                   If        g02err = '1'
141100050510     c                   Clear                   datarz
141200050510     c                   Else
141300050510     c                   Move      g02dat        datarz
141400050510     c                   EndIf
141500050509     c                   EndIf
141600050509
141700050509      * Imposto i campi come devono essere stampati
141800050509      * data/tipo doc/descrizione
141900050509     c                   Eval      rigadoc1 = %editw(datarz:'  /  /    ') +
142000050509     c                             ' ' + dcccdd + ' ' + §cdddesc
142100050510     c                   Eval      s_rigadoc(conta) = rigadoc1
142200050509
142300050509      * Cerco i dati previsti per il documento
142400050509     c                   If        §cddpnum = 'S'
142500050509      * e se è già pervenuto altrimenti non faccio vedere i campi vuoti
142600050509     c                             and dccdrz > 0
142700050510
142800050510     c                   Add       1             conta
142900050509      * data documento
143000050509     c                   If        dclddc > *Zeros
143100050509     c     *iso          Move      dclddc        wdataeur
143200050509     c     *eur          Movel     wdataeur      datadc
143300050510     c                   EndIf
143400050509      * Imposto i campi come devono essere stampati
143500050509      * numero doc./data/importo/valuta
143600050510     c                   Eval      rigadoc1 = w014a + 'Numero:' + ' ' +
143700050509     c                                      %trim(%editc(dclndc:'4')) + ' ' +
143800050509     c                                     'Data documento:' + ' ' +
143900050509     c                                      %trim(%editw(datadc:'  /  /    ')) +
144000050509     c                                     ' ' + 'Importo:' + ' ' +
144100050509     c                                      %trim(%editc(dclipr:'4')) + ' ' +
144200050509     c                                      dclvpr
144300050510     c                   Eval      s_rigadoc(conta) = rigadoc1
144400050509     c                   EndIf
144500050509
144600050509     c                   EndDo
144700050509
144800050509      * Stampo i documenti su due colonne
144900050509     c     conta         div       2             righe
145000050509     c                   mvr                     resto
145100050509     c                   add       resto         righe
145200050509     c                   Clear                   col1
145300050509     c                   Clear                   col2
145400050509     c                   Do        righe         col1
145500050509     c                   Eval      col2 = col1 + righe
145600050509     c                   Clear                   dn91d16
145700050509     c                   Eval      rigadoc1 = s_rigadoc(col1)
145800050509     c                   Eval      rigadoc2 = s_rigadoc(col2)
145900050509      * --> STAMPO 16° record
146000050517     c                   Add       1             curline
146100050517     c                   ExSr      Sr_Over
146200050518     c                   If        wover = *On
146300050518     c                   Write     dn91d15
146400050518     c                   EndIf
146500050509     c                   Write     dn91d16
146600050509     c                   EndDo
146700050509     c                   EndIf
146800050516
146900050516      *
    Note generiche                     ?
147000050516     c                   Eval      kdcstpd = 'C'
147100050516     c                   Movel(p)  dsnumca       kdcsnkt
147200050516     c                   Clear                   kdcsstd
147300050517     c                   Eval      conta = 1
147400050517     c                   Eval      wgiro = 1
147500050517     c                   Clear                   oldhim
147600050516     c     kFndcs02      Setll     Fndcs01l
147700050516     c                   If        %Equal(Fndcs01l)
147800050516      * --> STAMPO 17° record
147900050516      *     intestazione note
148000050517     c                   Add       3             curline
148100050517     c                   ExSr      Sr_Over
148200050516     c                   Write     dn91d17
148300050516
148400050516     c                   Do        *Hival
148500050516     c     kFndcs02      Reade     Fndcs01l
148600050516     c                   If        %Eof(Fndcs01l)
148700050516     c                   Leave
148800050516     c                   EndIf
148900050516     c                   If        dcsatb = 'A'
149000050516     c                   Iter
149100050516     c                   EndIf
149200050517
149300050517     c                   Eval      *In07 = *Off
149400050516
149500050517      * se stessa data/ora immissione ed ho già caricato 4 note nel campo nota
149600050517      * stampo
149700050517     c                   If        conta = 5 and
149800050517     c                             (oldhim = dcshim and oldhim > *Zeros)
149900050517      * se non è la prima volta che stampo non stampo la data/ora immissione e la fase
150000050517     c                   If        wgiro > 1
150100050517     c                   Eval      *In07 = *On
150200050517     c                   EndIf
150300050517      * se è la prima volta che stampo devo stampare la data/ora immissione e la fase
150400050517     c                   If        wgiro = 1
150500050517     c                   Add       1             wgiro
150600050517     c                   EndIf
150700050517     c                   Eval      riganota = %trim(riganota)
150800050517      * --> STAMPO 18° record
150900050517     c                   Add       1             curline
151000050517     c                   ExSr      Sr_Over
151100050518     c                   If        wover = *On
151200050518     c                   Write     dn91d17
151300050518     c                   EndIf
151400050517     c                   Write     dn91d18
151500050517     c                   Clear                   riganota
151600050517     c                   Eval      conta = 1
151700050517     c                   EndIf
151800050517
151900050517      * a rottura di ora immissione stampo le note
152000050517     c                   If        oldhim <> dcshim
152100050517     c                   If        oldhim > *Zeros
152200050517      * se non è la prima volta che stampo non stampo la data/ora immissione e la fase
152300050517     c                   If        wgiro > 1
152400050517     c                   Eval      *In07 = *On
152500050517     c                   EndIf
152600050517     c                   Eval      riganota = %trim(riganota)
152700050517      * --> STAMPO 18° record
152800050517     c                   Add       1             curline
152900050517     c                   ExSr      Sr_Over
153000050518     c                   If        wover = *On
153100050518     c                   Write     dn91d17
153200050518     c                   EndIf
153300050517     c                   Write     dn91d18
153400050517     c                   Clear                   riganota
153500050517     c                   Eval      conta = 1
153600050517     c                   Eval      wgiro = 1
153700050517     c                   EndIf
153800050517     c                   Eval      oldhim = dcshim
153900050517     c                   EndIf
154000050516
154100050516     c                   Clear                   datanot
154200050516      * Data nota
154300050516     c     *iso          Move      dcsdim        wdataeur
154400050516     c     *eur          Movel     wdataeur      datanot
154500050516
154600050516      * Fase nota
154700050516     c                   Eval      pfaseno = %subst(dcsnks:1:3)
154800050517      * P.o. nota e utente
154900050517     c                   Eval      pdcspos = dcspos
155000050517     c                   Eval      pdcspru = dcspru
155100050516
155200050516      * Imposto la riga della nota 4 note x riga
155300050517     c                   Eval      riganota = %trim(riganota) + ' ' + dcsnot
155400050517     c                   Add       1             conta
155500050516
155600050516     c                   EndDo
155700050517
155800050517      * se a fine lettura ho ancora delle note da stampare le stampo
155900050517     c                   If        conta > 1
156000050517     c                   If        wgiro > 1
156100050517     c                   Eval      *In07 = *On
156200050517     c                   EndIf
156300050517     c                   Eval      riganota = %trim(riganota)
156400050517     c                   Add       1             curline
156500050517     c                   ExSr      Sr_Over
156600050518     c                   If        wover = *On
156700050518     c                   Write     dn91d17
156800050518     c                   EndIf
156900050517     c                   Write     dn91d18
157000050517     c                   EndIf
157100050516
157200050516     c                   EndIf
157300050408
157400050411     c                   EndSr
157500050517
157600050517      *------------------------------------------------------------------------*
157700050517      * GESTIONE SALTO PAGINA
157800050517      *------------------------------------------------------------------------*
157900050517     c     Sr_Over       BegSr
158000050517
158100050518     c                   Eval      wover = *Off
158200050517     c                   If        *In98 or curline >= 63
158300050517     c                   Write     dn91t1
158400050517     c                   Write     dn91d1
158500050517     c                   Eval      *In98 = *Off
158600050518     c                   Eval      wover = *On
158700050517     c                   EndIf
158800050517
158900050517     c                   EndSr
159000050414
159100050414      *------------------------------------------------------------------------*
159200050414      *  PULIZIA CAMPI
159300050414      *------------------------------------------------------------------------*
159400050414     c     Pulisci       BegSr
159500050414
159600050415     c                   Clear                   dn91d1
159700050415     c                   Clear                   dn91d2
159800050415     c                   Clear                   dn91d3
159900050415     c                   Clear                   dn91d4
160000050415     c                   Clear                   dn91d5
160100050415     c                   Clear                   dn91d6
160200050418     c                   Clear                   dn91d7
160300050418     c                   Clear                   dn91d8
160400050418     c                   Clear                   dn91d9
160500050418     c                   Clear                   dn91d10
160600050418     c                   Clear                   dn91d11
160700050418     c                   Clear                   dn91d12
160800090414     c*****              clear                   dn91d20
160900090414     c*****              clear                   dn91d21
161000090414     c*****              clear                   dn91d22
161100050418     c                   Clear                   dn91d13
161200050418     c                   Clear                   dn91d14
161300050504     c                   Clear                   dn91d15
161400050504     c                   Clear                   dn91d16
161500050516     c                   Clear                   dn91d17
161600050516     c                   Clear                   dn91d18
161700050414
161800050414     c                   EndSr
161900050414
162000050418      *------------------------------------------------------------------------*
162100050418      *  PULIZIA CAMPI FILE FNDCL
162200050418      *------------------------------------------------------------------------*
162300050418     c     Puldcl        BegSr
162400050418
162500050418     c                   Clear                   dclipr
162600050418     c                   Clear                   dclvpr
162700050418     c                   Clear                   dclipt
162800050418     c                   Clear                   dclvpt
162900050418     c                   Clear                   dcldpt
163000050418     c                   Clear                   dclipl
163100050418     c                   Clear                   dclvpl
163200050418     c                   Clear                   dclndc
163300050418     c                   Clear                   dclddc
163400050418     c                   Clear                   dclvlp
163500050418     c                   Clear                   dcldpl
163600050418     c                   Clear                   dcldds
163700050418     c                   Clear                   dclabi
163800050418     c                   Clear                   dclcab
163900050418     c                   Clear                   dclasn
164000050418     c                   Clear                   dcldea
164100050418
164200050418     c                   EndSr
164300050418
164400050411      *------------------------------------------------------------------------*
164500050411      *  CONTROLLO TIPO BOLLA
164600050411      *------------------------------------------------------------------------*
164700050411     c     Ctrtbl        BegSr
164800050411
164900050411     c                   Clear                   dstb
165000050412     c                   Eval      ktblcod = 'TB'
165100050412     c                   Eval      ktblkey = tastbl
165200050411     c     kTab          Chain     Tabel00f
165300050411     c                   If        %Found(tabel00f)
165400050411     c                   Eval      dstb = tbluni
165500050411     c                   If        §tbrbl <> 'R'
165600050411     c                   Eval      flgtb1 = §tbtpo
165700050411     c                   EndIf
165800050411     c                   EndIf
165900050411
166000050411     c                   EndSr
166100050414
166200050411      *------------------------------------------------------------------------*
166300050411      * ROUTINE INIZIALE
166400050411      *------------------------------------------------------------------------*
166500050411     c     *Inzsr        BegSr
166600050411
166700050411     c     *Entry        Plist
166800050411     c                   Parm                    kpjba
166900050411     c                   Movel     kpjbu         parm01
167000050414
167100050418     c                   If        wflg = *Blanks
167200050418     c                   Eval      *In10 = *Off
167300050418     c                   Clear                   waac
167400050418     c                   Clear                   wfil
167500050418     c                   Clear                   wnca
167600050418     c                   Else
167700050418     c                   Eval      *In10 = *On
167800050418     c                   EndIf
167900050411
168000050411     c     *dtaara       define    §azute        azuteds
168100050411     c     *dtaara       define    §datiute      ddatiute
168200050411
168300050411     c                   in(E)     *dtaara
168400050411     c                   IF        %ERROR or RSUT = *blanks
168500050411     c                   clear                   Tibs34Ds
168600050411     c                   call      'TIBS34R'
168700050411     c                   parm                    Tibs34Ds
168800050411     c                   in        *dtaara
168900050411     c                   ENDIF
169000050412
169100050412     c                   Z-add     *date         wdata
169200150617
169300150617      /free
169400150617       //?Apro il file di stampa
169500150617         open FIDN91P;
169600150617      /end-free
169700050414
169800050414     c     kwndc         klist
169900050414     c                   kfld                    waac
170000050414     c                   kfld                    wfil
170100050414     c                   kfld                    wnca
170200050411
170300050411     c     kTitas        Klist
170400050411     c                   Kfld                    dctaas
170500050411     c                   Kfld                    dctlnp
170600050411     c                   Kfld                    dctnrs
170700050411     c                   Kfld                    dctnsp
170800050503
170900050503     c     kTita4        Klist
171000050503     c                   Kfld                    dctaas
171100050503     c                   Kfld                    dctlnp
171200050503     c                   Kfld                    dctnrs
171300050503     c                   Kfld                    dctnsp
171400050503     c                   kfld                    ktrc
171500050411
171600050411     c     kTab          klist
171700050411     c                   kfld                    codut
171800050412     c                   kfld                    ktblcod
171900050412     c                   kfld                    ktblkey
172000050412
172100050412     c     kFndcs        klist
172200050412     c                   kfld                    kdcstpd
172300050412     c                   kfld                    kdcsnkt
172400050412     c                   kfld                    kdcsstd
172500050412     c                   kfld                    kdcsdim
172600050412     c                   kfld                    kdcshim
172700050412     c                   kfld                    kdcsnks
172800050412     c                   kfld                    kdcstrc
172900050415
173000050415     c     kFndcs01      klist
173100050415     c                   kfld                    kdcstpd
173200050415     c                   kfld                    kdcsnkt
173300050415     c                   kfld                    kdcsstd
173400050415     c                   kfld                    kdcsdim
173500050415     c                   kfld                    kdcshim
173600050415     c                   kfld                    kdcsnks
173700050415     c                   kfld                    kdcstrc
173800050415     c                   kfld                    kdcspno
173900050516
174000050516     c     kFndcs02      klist
174100050516     c                   kfld                    kdcstpd
174200050516     c                   kfld                    kdcsnkt
174300050516     c                   kfld                    kdcsstd
174400050412
174500050412     c     kFndet        klist
174600050412     c                   kfld                    dctaae
174700050412     c                   kfld                    dctnev
174800050503
174900050503     c     kFndet2       klist
175000050503     c                   kfld                    dctaad
175100050503     c                   kfld                    dctpod
175200050503     c                   kfld                    dctnde
175300050412
175400050412     c     kFndka        klist
175500050412     c                   kfld                    dctaac
175600050412     c                   kfld                    dctfil
175700050412     c                   kfld                    dctnca
175800050412     c                   kfld                    wtrc
175900050411
176000050414     c     kFndc         klist
176100050413     c                   kfld                    dctaac
176200050413     c                   kfld                    dctfil
176300050413     c                   kfld                    dctnca
176400050414
176500050414     c     kCnabi        klist
176600050414     c                   kfld                    wabi
176700050414     c                   kfld                    wcab
176800050413
176900050411     c                   EndSr
