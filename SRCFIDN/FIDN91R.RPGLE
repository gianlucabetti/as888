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
002400050413     fFidn91p   o    e             printer  oflind(*in98) infds(prtline)
002500050408
002600050408      *------------------------------------------------------------------------*
002700050408      *  RIEPILOGO INDICATORI
002800050408      *------------------------------------------------------------------------*
002900050517      * 01   - Stampo Rivalsa
003000050517      * 02   - Stampo Responsabilità Internazionale
003100050517      * 03   - Stampo Importo da assicurare
003200050517      * 04   - Stampo Evento
003300050517      * 05   - Stampo Mandato
003400050517      * 06   - Stampo Note calcolo prg. liquidazione
003500050517      * 07   - Non stampo data/ora immissione e la fase nota generica
003600070104      * 08   - Non stampo data ricezione disposizioni A.I.G.
003700050415      * 10   - Stampo la c.a. passata in kpjbu
003800050415      * 20   - Liquidazione transattiva
003900050415      * 21   - Pratica
004000050415      * 22   - Pratica in franchigia
004100160302      * 23   - Pagamento tramite bonifico
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
007400050517     d oldhim          s                   like(dcshim)
007500050504     d resto           s              3  0
007600050504     d righe           s              3  0
007700050414     d wabi            s                   like(AbiAbi)
007800050414     d wcab            s                   like(AbiCab)
007900050414     d wdataeur        s               d   datfmt(*eur)
008000050412     d wdcsnot         s                   like(DcsNot)
008100050503     d wdoc            s              1    inz('0')
008200050415     d wgiro           s              3  0 inz
008300050415     d wnote           s              1    inz('0')
008400050518     d wover           s              1    inz('0')
008500050412     d wtrc            s                   like(DkaTrc)
008600050419     d w001a           s              1
008700050412     d w0040           s              4  0
008800050412     d w0080           s              8  0
008900050510     d w014a           s             14
009000050408
009100050408      *------------------------------------------------------------------------*
009200050408      *   D S   I N T E R N E / E S T E R N E
009300050408      *------------------------------------------------------------------------*
009400050408     d parm01          ds
009500050516     d  waaca                         4  0
009600050418     d  wflg                          1
009700050414     d  waac                          4  0
009800050414     d  wfil                          3  0
009900050414     d  wnca                          7  0
010000050413
010100050413     d prtline         ds
010200050413     d  curline              367    368i 0
010300050412
010400050412     d                 ds
010500050412     d  dctaac                 1      4  0
010600050412     d  dctfil                 5      7  0
010700050412     d  dctnca                 8     14  0
010800050412     d  dsnumca                1     14  0
010900050414
011000050428     d                 ds
011100050503     d  wdctaac                1      4  0
011200050428     d  dctmgc                 5      8  0
011300050428     d  wdataca                1      8  0
011400050428
011500050414     d wlbdat          ds                  inz
011600050414     d  g02dat                 1      8  0
011700050414     d  g02inv                 9     16  0
011800050414     d  g02err                17     17
011900050414     d  g02tgi                18     22  0
012000050408
012100050412     d azuteds       e ds                  extname(AZUTE00F)
012200050412     d dcch          e ds
012300050414     d dcdd          e ds
012400050412     d ddatiute      e ds
012500050414     d ddcf01        e ds
012600050411     d ddct01        e ds
012700050411     d dstb          e ds
012800050414     d dstr          e ds
012900050414     d ds1p          e ds
013000050411     d dtad          e ds
013100050428     d dta4a         e ds
013200050412     d ds_cnaco      e ds                  extname(CNACO00F)
013300050412     d ds_cnind      e ds                  extname(CNIND00F)
013400050412     d ds_cnclp      e ds                  extname(CNCLP00F)
013500050412     d ds_fncls      e ds                  extname(FNCLS00F)
013600050408     d kpjba         e ds
013700050412     d tibs02ds      e ds
013800050412     d tibs34ds      e ds                  inz
013900050412     d tibs69ds      e ds
014000050408
014100050408      *------------------------------------------------------------------------*
014200050414
014300050414     c   10kwndc         Setll     Fndct01l
014400050516     c  n10waaca         Setll     Fndct01l
014500050411     c                   Do        *Hival
014600050414     c   10kwndc         Reade     Fndct01l
014700050516     c  n10waaca         Reade     Fndct01l
014800050415
014900050411      * Fine file
015000050516     c                   If        %Eof(Fndct01l)
015100050411     c                   Leave
015200050411     c                   EndIf
015300050503
015400050503     c                   Eval      wdctaac = dctaac
015500050415
015600050415      * Pulizia dei record di stampa
015700050415     c                   ExSr      Pulisci
015800050415
015900050415      * Valorizzo i flag operativi
016000050415     c                   Eval      ddct01 = dctflo
016100050412
016200050415      * Stampo una c.a. x pagina
016300050606     c                   Clear                   page
016400050517     c                   Add       1             curline
016500050413     c                   Write     dn91t1
016600050411
016700050415      * Recupero i dati e stampo il dettaglio della c.a.
016800050411     c                   ExSr      Elabora
016900050411
017000050411     c                   EndDo
017100050411
017200050411     c                   Eval      *InLr = *On
017300050414
017400050411      *------------------------------------------------------------------------*
017500050411      * ELABORO LA C.A.
017600050411      *------------------------------------------------------------------------*
017700050411     c     Elabora       BegSr
017800050415
017900050429      *
    C.A.                               ?
018000050415     c                   Eval      pdctfil = dctfil
018100050415     c                   Eval      pdctnca = dctnca
018200050415      * Data della c.a.
018300050412     c                   Move      dctmgc        w0080
018400050412     c                   Movel     dctaac        w0080
018500050414     c     *iso          Move      w0080         wdataeur
018600050414     c     *eur          Movel     wdataeur      dataca
018700050415
018800050429      *
    Pratica assicurativa               ?
018900050415     c                   Eval      pdctdit = dctdit
019000050415     c                   Eval      pdctpra = dctpra
019100050415     c                   Eval      pdctprn = dctprn
019200050429
019300050429      *
    Tipo gestione                      ?
019400050429     c                   Eval      *In20 = *Off
019500050429     c                   Eval      *In21 = *Off
019600050429     c                   Eval      *In22 = *Off
019700050429     c                   Select
019800050429     c                   When      dctfpr = 'T'
019900050429     c                   Eval      desfpr = 'LIQUIDAZIONE TRANSATTIVA'
020000050429     c                   Eval      tdespag = 'ESTREMI PAGAMENTO L.T.:'
020100050429     c                   Eval      *In20 = *On
020200050429     c                   When      dctfpr = 'P' and §dctpfra <> 'F'
020300050429     c                   Eval      desfpr = 'PRATICA ASSICURATIVA'
020400050429     c                   Eval      tdespag = 'ESTREMI PAGAMENTO PRATICA:'
020500050429     c                   Eval      *In21 = *On
020600050429     c                   When      dctfpr = 'P' and §dctpfra = 'F'
020700050429     c                   Eval      desfpr = 'PRATICA ASSICURAT.IN FRANCHIGIA'
020800050429     c                   Eval      tdespag = 'ESTREMI PAGAMENTO PRATICA IN FRAN-
020900050429     C                             CHIGIA:'
021000050429     c                   Eval      *In22 = *On
021100050429     c                   Other
021200050429     c                   Eval      tdespag = 'ESTREMI PAGAMENTO:'
021300050429     c                   EndSl
021400050429      * Gestione con Mittente / Destinatario
021500050429     c                   If        dctptr = 'M'
021600050429     c                   Eval      desfpr = %trim(desfpr) + ' con MITTENTE'
021700050429     c                   EndIf
021800050429     c                   If        dctptr = 'D'
021900050429     c                   Eval      desfpr = %trim(desfpr) + ' con DESTINATARIO'
022000050429     c                   EndIf
022100050415
022200050429      *
    Denuncia                           ?
022300050415     c                   Eval      pdctaad = dctaad
022400050415     c                   Eval      pdctpod = dctpod
022500050415     c                   Eval      pdctnde = dctnde
022600050428      * cerco la data di stampa della denuncia
022700050428     c                   If        dctnde > *Zeros
022800050428     c     kFndet2       Chain     Fndet02l
022900050428     c                   If        %Found(Fndet02l)
023000050428      * data di stampa della denuncia
023100050428     c                   Move      detdsb        w0080
023200050428     c     *iso          Move      w0080         wdataeur
023300050428     c     *eur          Movel     wdataeur      datade
023400050428     c                   EndIf
023500050428     c                   EndIf
023600050415
023700050428     c                   Eval      *In04 = *Off
023800050428
023900050429      *
    Evento                             ?
024000050415     c                   If        dctnev > *Zeros
024100050428     c                   Eval      *In04 = *On
024200050415     c                   Eval      pdctaae = dctaae
024300050415     c                   Eval      pdctnev = dctnev
024400050415      * reperisco i dati relativi all'evento
024500050415     c     kFndet        Chain     Fndet01l
024600050415     c                   If        %Found(Fndet01l)
024700050415      * data dell'evento
024800050415     c                   Move      detmge        w0080
024900050415     c                   Movel     detaae        w0080
025000050415     c     *iso          Move      w0080         wdataeur
025100050415     c     *eur          Movel     wdataeur      dataev
025200050415      * tipo anomalia evento
025300050415     c                   Clear                   dtad
025400050415     c                   Clear                   tibs02ds
025500050415     c                   Eval      t02mod = 'C'
025600050415     c                   Eval      t02sif = knsif
025700050415     c                   Eval      t02cod = 'TAD'
025800050415     c                   Eval      t02ke1 = dettad
025900050415     c                   Call      'TIBS02R'
026000050415     c                   Parm                    kpjba
026100050415     c                   Parm                    tibs02ds
026200050415     c                   If        t02err = *Blanks
026300050415     c                   Eval      dtad = t02uni
026400050415     c                   EndIf
026500050415     c                   Eval      destev = §taddesc
026600050415      * descrizione aggiuntiva evento
026700050415     c                   Eval      kdcstpd = 'C'
026800050415     c                   Movel(p)  dsnumca       kdcsnkt
026900050415     c                   Eval      kdcsstd = 'E'
027000050415     c                   Clear                   kdcsdim
027100050415     c                   Clear                   kdcshim
027200050415     c                   Clear                   kdcsnks
027300050415     c                   Eval      kdcstrc = 'D'
027400050415     c     kFndcs        Chain     Fndcs01l
027500050415     c                   If        %Found(Fndcs01l)
027600050415     c                   Eval      deseva = dcsnot
027700050415     c                   EndIf
027800050415     c                   EndIf
027900050415     c                   EndIf
028000050415
028100050429      *
    Spedizione                         ?
028200050415     c     kTitas        Chain     Tntmd01l
028300090122     c                   if        not %found(tntmd01l)
028400090122     c                   clear     *all          tntmd000
028500090122     c                   endif
028600050415     c     kTitas        Chain     Titas30c
028700090122     c                   if        not %found(titas30c)
028800090122     c                   clear     *all          titas000
028900090122     c                   endif
029000050415     c                   ExSr      Ctrtbl
029100050415     c                   If        flgtb1 = *Blanks
029200050415     c     kTitas        Reade     Titas30c
029300050415     c                   EndIf
029400050415      * mittente/destinario
029500050415     c                   If        §dctport = 'A'
029600050415     c                   Eval      climit = tasccm
029700050415     c                   Eval      clides = tasksc
029800050415     c                   Else
029900050415     c                   Eval      climit = tasksc
030000050415     c                   Clear                   clides
030100050415     c                   EndIf
030200050415      * indirizzo mittente
030300050415     c                   Eval      ptmdrsm = tmdrsm
030400050516     c                   Eval      deslom = %trim(tmdinm) + ' ' + '-' + ' ' +
030500050516     c                                      %trim(tmdcam) + ' ' + %trim(tmdlom)
030600050516     c                                      + '  ' + tmdprm + ' ' + tmdnzm
030700050415      * indirizzo destinatario
030800050415     c                   Eval      ptmdrsd = tmdrsd
030900050516     c                   Eval      deslod = %trim(tmdind) + ' ' + '-' + ' ' +
031000050516     c                                      %trim(tmdcad) + ' ' + %trim(tmdlod)
031100050516     c                                      + '  ' + tmdprd + ' ' + tmdnzd
031200050415      * numero spedizone
031300050415     c                   Eval      pdctlnp = dctlnp
031400050415     c                   Eval      pdctnrs = dctnrs
031500050415     c                   Eval      pdctnsp = dctnsp
031600050415      * data della spedizione
031700050415     c                   Move      tasmgs        w0080
031800050415     c                   Movel     tasaas        w0080
031900090331      * se data a 0 pulisco il campo data spedizione
032000090331     c                   if        w0080 = *zeros
032100090331     c                   clear                   datasp
032200090331     c                   else
032300050415     c     *iso          Move      w0080         wdataeur
032400050415     c     *eur          Movel     wdataeur      datasp
032500090331     c                   endif
032600050415      * tipo bolla assegnato/franco
032700050411     c                   If        §dctport = 'F'
032800050411     c                   Eval      descbo = 'FRANCO'
032900050411     c                   Else
033000050411     c                   Eval      descbo = 'ASSEGNATO'
033100050411     c                   EndIf
033200050415      * linea di arrivo
033300050415     c                   Eval      pdctlna = dctlna
033400050415      * riferimento mittente
033500050415     c                   Eval      ptasrmn = tasrmn
033600050428      * riferimento mittente alfanumerico
033700050428     c                   Eval      ktrc = 'A'
033800050428     c     ktita4        Chain     Tita430c
033900050428     c                   If        %Found(Tita430c)
034000050428     c                   Eval      dta4a = ta4not
034100050428     c                   Eval      pta4rma = §ta4arma
034200050428     c                   EndIf
034300050415      * colli/peso/volume
034400050415     c                   Eval      ptasncl = tasncl
034500050415     c                   Eval      ptaspkf = taspkf
034600050415     c                   Eval      ptasfvf = tasfvf
034700050415     c                   Eval      ptasvlf = tasvlf
034800050415      * natura merce
034900050415     c                   Eval      pdctnas = dctnas
035000050415
035100050415      * --> STAMPO 1° record
035200050517     c  n04              Add       5             curline
035300050517     c   04              Add       6             curline
035400050415     c                   Write     dn91d1
035500050412
035600050429      *
    Anomalia                           ?
035700050418     c                   Eval      pdcttad = dcttad
035800050415      * decodifico
035900050412     c                   Clear                   dtad
036000050412     c                   Clear                   tibs02ds
036100050412     c                   Eval      t02mod = 'C'
036200050412     c                   Eval      t02sif = knsif
036300050412     c                   Eval      t02cod = 'TAD'
036400050412     c                   Eval      t02ke1 = dcttad
036500050412     c                   Call      'TIBS02R'
036600050412     c                   Parm                    kpjba
036700050412     c                   Parm                    tibs02ds
036800050412     c                   If        t02err = *Blanks
036900050412     c                   Eval      dtad = t02uni
037000050412     c                   EndIf
037100050415     c                   Eval      destad = §taddesc
037200050415     c                   Eval      wnote = *Off
037300050415      * cerco le descrizioni aggiuntive
037400050412     c                   Eval      kdcstpd = 'C'
037500050412     c                   Movel(p)  dsnumca       kdcsnkt
037600050412     c                   Eval      kdcsstd = 'A'
037700050412     c                   Clear                   kdcsdim
037800050412     c                   Clear                   kdcshim
037900050412     c                   Clear                   kdcsnks
038000050412     c                   Eval      kdcstrc = 'D'
038100050415     c                   Eval      conta = 1
038200050415     c                   Eval      wgiro = 1
038300050415     c     kFndcs        Setll     Fndcs01l
038400050415     c                   Do        *Hival
038500050415     c     kFndcs        Reade     Fndcs01l
038600050415     c                   If        %Eof(Fndcs01l)
038700050415     c                   Leave
038800050415     c                   EndIf
038900050415     c                   Eval      wnote = *On
039000050415     c                   Select
039100050415      * se primo giro devo stampare 4 note assieme
039200050415     c                   When      wgiro = 1
039300050415      * se ho già caricato 4 note salvo la chiave della nota che sto leggendo x poi riposizionarmi
039400050415     c                   If        conta = 5
039500050415     c                   Eval      kdcstpd = dcstpd
039600050415     c                   Eval      kdcsnkt = dcsnkt
039700050415     c                   Eval      kdcsstd = dcsstd
039800050415     c                   Eval      kdcsdim = dcsdim
039900050415     c                   Eval      kdcshim = dcshim
040000050415     c                   Eval      kdcsnks = dcsnks
040100050415     c                   Eval      kdcstrc = dcstrc
040200050415     c                   Eval      kdcspno = dcspno
040300050415      * --> STAMPO 2° record
040400050517     c                   Eval      destag = %trim(destag)
040500050517     c                   Add       1             curline
040600050415     c                   Write     dn91d2
040700050415      * riporto il contatore a 1
040800050415     c                   Eval      conta = 1
040900050415     c                   Eval      wgiro = 2
041000050415      * mi riposiziono sul file delle note all'ultima nota letta e torno a leggere
041100050415     c     kFndcs01      Setll     Fndcs01l
041200050415     c                   Iter
041300050415     c                   EndIf
041400050428      * se non è il primo giro devo stampare 4 note assieme
041500050415     c                   Other
041600050428      * se ho già caricato 4 note salvo la chiave della nota che sto leggendo x poi riposizionarmi
041700050428     c                   If        conta = 5
041800050415     c                   Eval      kdcstpd = dcstpd
041900050415     c                   Eval      kdcsnkt = dcsnkt
042000050415     c                   Eval      kdcsstd = dcsstd
042100050415     c                   Eval      kdcsdim = dcsdim
042200050415     c                   Eval      kdcshim = dcshim
042300050415     c                   Eval      kdcsnks = dcsnks
042400050415     c                   Eval      kdcstrc = dcstrc
042500050415     c                   Eval      kdcspno = dcspno
042600050415      * --> STAMPO 3° record
042700050517     c                   Eval      desta1 = %trim(desta1)
042800050517     c                   Add       1             curline
042900050517     c                   ExSr      Sr_Over
043000050518     c                   If        wover = *On
043100050518     c                   Write     dn91d19
043200050518     c                   EndIf
043300050415     c                   Write     dn91d3
043400050418     c                   Clear                   desta1
043500050415      * riporto il contatore a 1
043600050415     c                   Eval      conta = 1
043700050415      * mi riposiziono sul file delle note all'ultima nota letta e torno a leggere
043800050415     c     kFndcs01      Setll     Fndcs01l
043900050415     c                   Iter
044000050415     c                   EndIf
044100050415     c                   Endsl
044200050415      * campi diversi se primo giro o giri sucessivi
044300050415     c                   If        wgiro = 1
044400050418     c                   Eval      destag = %trim(destag) + ' ' + dcsnot
044500050415     c                   Else
044600050418     c                   Eval      desta1 = %trim(desta1) + ' ' + dcsnot
044700050415     c                   EndIf
044800050415      * incremento il contatore delle note lette
044900050415     c                   Add       1             conta
045000050415     c                   EndDo
045100050415     c                   Select
045200050415      * --> STAMPO ultimo record note
045300050415     c                   When      wnote = *On
045400050517     c                   If        wgiro = 1 and conta > 1
045500050415     c                             and wnote = *On
045600050517     c                   Eval      destag = %trim(destag)
045700050517     c                   Add       1             curline
045800050517     c                   ExSr      Sr_Over
045900050518     c                   If        wover = *On
046000050518     c                   Write     dn91d19
046100050518     c                   EndIf
046200050415     c                   Write     dn91d2
046300050415     c                   EndIf
046400050517     c                   If        wgiro > 1 and conta > 1
046500050415     c                             and wnote = *On
046600050517     c                   Eval      desta1 = %trim(desta1)
046700050517     c                   Add       1             curline
046800050517     c                   ExSr      Sr_Over
046900050518     c                   If        wover = *On
047000050518     c                   Write     dn91d19
047100050518     c                   EndIf
047200050418     c                   Write     dn91d3
047300050415     c                   EndIf
047400050415      * --> STAMPO 2° record (la riga dell'anomalia) se non ho trovato descrizioni aggiuntive
047500050415     c                   other
047600050517     c                   Add       1             curline
047700050415     c                   Write     dn91d2
047800050415     c                   EndSl
047900050428
048000050429      *
    Motivo del danno                   ?
048100050428     c                   Eval      wnote = *Off
048200050428      * cerco le descrizioni aggiuntive
048300050428     c                   Eval      kdcstpd = 'C'
048400050428     c                   Movel(p)  dsnumca       kdcsnkt
048500050428     c                   Eval      kdcsstd = 'M'
048600050428     c                   Clear                   kdcsdim
048700050428     c                   Clear                   kdcshim
048800050428     c                   Clear                   kdcsnks
048900050428     c                   Eval      kdcstrc = 'D'
049000050428     c                   Eval      conta = 1
049100050428     c                   Eval      wgiro = 1
049200050428     c     kFndcs        Setll     Fndcs01l
049300050428     c                   Do        *Hival
049400050428     c     kFndcs        Reade     Fndcs01l
049500050428     c                   If        %Eof(Fndcs01l)
049600050428     c                   Leave
049700050428     c                   EndIf
049800050428     c                   Eval      wnote = *On
049900050428     c                   Select
050000050428      * se primo giro devo stampare 4 note assieme
050100050428     c                   When      wgiro = 1
050200050428      * se ho già caricato 4 note salvo la chiave della nota che sto leggendo x poi riposizionarmi
050300050428     c                   If        conta = 5
050400050428     c                   Eval      kdcstpd = dcstpd
050500050428     c                   Eval      kdcsnkt = dcsnkt
050600050428     c                   Eval      kdcsstd = dcsstd
050700050428     c                   Eval      kdcsdim = dcsdim
050800050428     c                   Eval      kdcshim = dcshim
050900050428     c                   Eval      kdcsnks = dcsnks
051000050428     c                   Eval      kdcstrc = dcstrc
051100050428     c                   Eval      kdcspno = dcspno
051200050428      * --> STAMPO 4° record
051300050517     c                   Eval      desmtv = %trim(desmtv)
051400050517     c                   Add       1             curline
051500050517     c                   ExSr      Sr_Over
051600050518     c                   If        wover = *On
051700050518     c                   Write     dn91d19
051800050518     c                   EndIf
051900050428     c                   Write     dn91d4
052000050428      * riporto il contatore a 1
052100050428     c                   Eval      conta = 1
052200050428     c                   Eval      wgiro = 2
052300050428      * mi riposiziono sul file delle note all'ultima nota letta e torno a leggere
052400050428     c     kFndcs01      Setll     Fndcs01l
052500050428     c                   Iter
052600050428     c                   EndIf
052700050518      * se non è il primo giro devo stampare 5 note assieme
052800050428     c                   Other
052900050518      * se ho già caricato 5 note salvo la chiave della nota che sto leggendo x poi riposizionarmi
053000050518     c                   If        conta = 6
053100050428     c                   Eval      kdcstpd = dcstpd
053200050428     c                   Eval      kdcsnkt = dcsnkt
053300050428     c                   Eval      kdcsstd = dcsstd
053400050428     c                   Eval      kdcsdim = dcsdim
053500050428     c                   Eval      kdcshim = dcshim
053600050428     c                   Eval      kdcsnks = dcsnks
053700050428     c                   Eval      kdcstrc = dcstrc
053800050428     c                   Eval      kdcspno = dcspno
053900050428      * --> STAMPO 5° record
054000050517     c                   Eval      desmtv1 = %trim(desmtv1)
054100050517     c                   Add       1             curline
054200050517     c                   ExSr      Sr_Over
054300050518     c                   If        wover = *On
054400050518     c                   Write     dn91d19
054500050518     c                   EndIf
054600050428     c                   Write     dn91d5
054700050428     c                   Clear                   desmtv1
054800050428      * riporto il contatore a 1
054900050428     c                   Eval      conta = 1
055000050428      * mi riposiziono sul file delle note all'ultima nota letta e torno a leggere
055100050428     c     kFndcs01      Setll     Fndcs01l
055200050428     c                   Iter
055300050428     c                   EndIf
055400050428     c                   Endsl
055500050428      * campi diversi se primo giro o giri sucessivi
055600050428     c                   If        wgiro = 1
055700050428     c                   Eval      desmtv = %trim(desmtv) + ' ' + dcsnot
055800050428     c                   Else
055900050428     c                   Eval      desmtv1 = %trim(desmtv1) + ' ' + dcsnot
056000050428     c                   EndIf
056100050428      * incremento il contatore delle note lette
056200050428     c                   Add       1             conta
056300050428     c                   EndDo
056400050428     c                   Select
056500050428      * --> STAMPO ultimo record note
056600050428     c                   When      wnote = *On
056700050517     c                   If        wgiro = 1 and conta > 1
056800050428     c                             and wnote = *On
056900050517     c                   Eval      desmtv = %trim(desmtv)
057000050517     c                   Add       1             curline
057100050517     c                   ExSr      Sr_Over
057200050518     c                   If        wover = *On
057300050518     c                   Write     dn91d19
057400050518     c                   EndIf
057500050428     c                   Write     dn91d4
057600050428     c                   EndIf
057700050517     c                   If        wgiro > 1 and conta > 1
057800050428     c                             and wnote = *On
057900050517     c                   Eval      desmtv1 = %trim(desmtv1)
058000050517     c                   Add       1             curline
058100050517     c                   ExSr      Sr_Over
058200050518     c                   If        wover = *On
058300050518     c                   Write     dn91d19
058400050518     c                   EndIf
058500050428     c                   Write     dn91d5
058600050428     c                   EndIf
058700050428     c                   EndSl
058800050415
058900050429      *
    Pezzi presenti                     ?
059000050429     c                   Eval      wnote = *Off
059100050429      * cerco le descrizioni aggiuntive
059200050429     c                   Eval      kdcstpd = 'C'
059300050429     c                   Movel(p)  dsnumca       kdcsnkt
059400050429     c                   Eval      kdcsstd = 'K'
059500050429     c                   Clear                   kdcsdim
059600050429     c                   Clear                   kdcshim
059700050429     c                   Clear                   kdcsnks
059800050429     c                   Eval      kdcstrc = 'D'
059900050429     c                   Eval      conta = 1
060000050429     c                   Eval      wgiro = 1
060100050429     c     kFndcs        Setll     Fndcs01l
060200050429     c                   Do        *Hival
060300050429     c     kFndcs        Reade     Fndcs01l
060400050429     c                   If        %Eof(Fndcs01l)
060500050429     c                   Leave
060600050429     c                   EndIf
060700050429     c                   Eval      wnote = *On
060800050429     c                   Select
060900050429      * se primo giro devo stampare 4 note assieme
061000050429     c                   When      wgiro = 1
061100050429      * se ho già caricato 4 note salvo la chiave della nota che sto leggendo x poi riposizionarmi
061200050429     c                   If        conta = 5
061300050429     c                   Eval      kdcstpd = dcstpd
061400050429     c                   Eval      kdcsnkt = dcsnkt
061500050429     c                   Eval      kdcsstd = dcsstd
061600050429     c                   Eval      kdcsdim = dcsdim
061700050429     c                   Eval      kdcshim = dcshim
061800050429     c                   Eval      kdcsnks = dcsnks
061900050429     c                   Eval      kdcstrc = dcstrc
062000050429     c                   Eval      kdcspno = dcspno
062100050429      * --> STAMPO 6° record
062200050517     c                   Eval      desprs = %trim(desprs)
062300050517     c                   Add       1             curline
062400050517     c                   ExSr      Sr_Over
062500050518     c                   If        wover = *On
062600050518     c                   Write     dn91d19
062700050518     c                   EndIf
062800050429     c                   Write     dn91d6
062900050429      * riporto il contatore a 1
063000050429     c                   Eval      conta = 1
063100050429     c                   Eval      wgiro = 2
063200050429      * mi riposiziono sul file delle note all'ultima nota letta e torno a leggere
063300050429     c     kFndcs01      Setll     Fndcs01l
063400050429     c                   Iter
063500050429     c                   EndIf
063600050518      * se non è il primo giro devo stampare 5 note assieme
063700050429     c                   Other
063800050518      * se ho già caricato 5 note salvo la chiave della nota che sto leggendo x poi riposizionarmi
063900050518     c                   If        conta = 6
064000050429     c                   Eval      kdcstpd = dcstpd
064100050429     c                   Eval      kdcsnkt = dcsnkt
064200050429     c                   Eval      kdcsstd = dcsstd
064300050429     c                   Eval      kdcsdim = dcsdim
064400050429     c                   Eval      kdcshim = dcshim
064500050429     c                   Eval      kdcsnks = dcsnks
064600050429     c                   Eval      kdcstrc = dcstrc
064700050429     c                   Eval      kdcspno = dcspno
064800050429      * --> STAMPO 7° record
064900050517     c                   Eval      desprs1 = %trim(desprs1)
065000050517     c                   Add       1             curline
065100050517     c                   ExSr      Sr_Over
065200050518     c                   If        wover = *On
065300050518     c                   Write     dn91d19
065400050518     c                   EndIf
065500050429     c                   Write     dn91d7
065600050429     c                   Clear                   desprs1
065700050429      * riporto il contatore a 1
065800050429     c                   Eval      conta = 1
065900050429      * mi riposiziono sul file delle note all'ultima nota letta e torno a leggere
066000050429     c     kFndcs01      Setll     Fndcs01l
066100050429     c                   Iter
066200050429     c                   EndIf
066300050429     c                   Endsl
066400050429      * campi diversi se primo giro o giri sucessivi
066500050429     c                   If        wgiro = 1
066600050429     c                   Eval      desprs = %trim(desprs) + ' ' + dcsnot
066700050429     c                   Else
066800050429     c                   Eval      desprs1 = %trim(desprs1) + ' ' + dcsnot
066900050429     c                   EndIf
067000050429      * incremento il contatore delle note lette
067100050429     c                   Add       1             conta
067200050429     c                   EndDo
067300050429     c                   Select
067400050429      * --> STAMPO ultimo record note
067500050429     c                   When      wnote = *On
067600050517     c                   If        wgiro = 1 and conta > 1
067700050429     c                             and wnote = *On
067800050517     c                   Eval      desprs = %trim(desprs)
067900050517     c                   Add       1             curline
068000050517     c                   ExSr      Sr_Over
068100050518     c                   If        wover = *On
068200050518     c                   Write     dn91d19
068300050518     c                   EndIf
068400050429     c                   Write     dn91d6
068500050429     c                   EndIf
068600050517     c                   If        wgiro > 1 and conta > 1
068700050429     c                             and wnote = *On
068800050517     c                   Eval      desprs1 = %trim(desprs1)
068900050517     c                   Add       1             curline
069000050517     c                   ExSr      Sr_Over
069100050518     c                   If        wover = *On
069200050518     c                   Write     dn91d19
069300050518     c                   EndIf
069400050429     c                   Write     dn91d7
069500050429     c                   EndIf
069600050429     c                   EndSl
069700050429
069800050429      *
    Pezzi mancanti                     ?
069900050429     c                   Eval      wnote = *Off
070000050429      * cerco le descrizioni aggiuntive
070100050429     c                   Eval      kdcstpd = 'C'
070200050429     c                   Movel(p)  dsnumca       kdcsnkt
070300050429     c                   Eval      kdcsstd = 'P'
070400050429     c                   Clear                   kdcsdim
070500050429     c                   Clear                   kdcshim
070600050429     c                   Clear                   kdcsnks
070700050429     c                   Eval      kdcstrc = 'D'
070800050429     c                   Eval      conta = 1
070900050429     c                   Eval      wgiro = 1
071000050429     c     kFndcs        Setll     Fndcs01l
071100050429     c                   Do        *Hival
071200050429     c     kFndcs        Reade     Fndcs01l
071300050429     c                   If        %Eof(Fndcs01l)
071400050429     c                   Leave
071500050429     c                   EndIf
071600050429     c                   Eval      wnote = *On
071700050429     c                   Select
071800050429      * se primo giro devo stampare 4 note assieme
071900050429     c                   When      wgiro = 1
072000050429      * se ho già caricato 4 note salvo la chiave della nota che sto leggendo x poi riposizionarmi
072100050429     c                   If        conta = 5
072200050429     c                   Eval      kdcstpd = dcstpd
072300050429     c                   Eval      kdcsnkt = dcsnkt
072400050429     c                   Eval      kdcsstd = dcsstd
072500050429     c                   Eval      kdcsdim = dcsdim
072600050429     c                   Eval      kdcshim = dcshim
072700050429     c                   Eval      kdcsnks = dcsnks
072800050429     c                   Eval      kdcstrc = dcstrc
072900050429     c                   Eval      kdcspno = dcspno
073000050429      * --> STAMPO 8° record
073100050517     c                   Eval      desnpz = %trim(desnpz)
073200050517     c                   Add       1             curline
073300050517     c                   ExSr      Sr_Over
073400050518     c                   If        wover = *On
073500050518     c                   Write     dn91d19
073600050518     c                   EndIf
073700050429     c                   Write     dn91d8
073800050429      * riporto il contatore a 1
073900050429     c                   Eval      conta = 1
074000050429     c                   Eval      wgiro = 2
074100050429      * mi riposiziono sul file delle note all'ultima nota letta e torno a leggere
074200050429     c     kFndcs01      Setll     Fndcs01l
074300050429     c                   Iter
074400050429     c                   EndIf
074500050518      * se non è il primo giro devo stampare 5 note assieme
074600050429     c                   Other
074700050518      * se ho già caricato 5 note salvo la chiave della nota che sto leggendo x poi riposizionarmi
074800050518     c                   If        conta = 6
074900050429     c                   Eval      kdcstpd = dcstpd
075000050429     c                   Eval      kdcsnkt = dcsnkt
075100050429     c                   Eval      kdcsstd = dcsstd
075200050429     c                   Eval      kdcsdim = dcsdim
075300050429     c                   Eval      kdcshim = dcshim
075400050429     c                   Eval      kdcsnks = dcsnks
075500050429     c                   Eval      kdcstrc = dcstrc
075600050429     c                   Eval      kdcspno = dcspno
075700050429      * --> STAMPO 9° record
075800050517     c                   Eval      desnpz1 = %trim(desnpz1)
075900050517     c                   Add       1             curline
076000050517     c                   ExSr      Sr_Over
076100050518     c                   If        wover = *On
076200050518     c                   Write     dn91d19
076300050518     c                   EndIf
076400050429     c                   Write     dn91d9
076500050429     c                   Clear                   desnpz1
076600050429      * riporto il contatore a 1
076700050429     c                   Eval      conta = 1
076800050429      * mi riposiziono sul file delle note all'ultima nota letta e torno a leggere
076900050429     c     kFndcs01      Setll     Fndcs01l
077000050429     c                   Iter
077100050429     c                   EndIf
077200050517     c                   EndSl
077300050429      * campi diversi se primo giro o giri sucessivi
077400050429     c                   If        wgiro = 1
077500050429     c                   Eval      desnpz = %trim(desnpz) + ' ' + dcsnot
077600050429     c                   Else
077700050429     c                   Eval      desnpz1 = %trim(desnpz1) + ' ' + dcsnot
077800050429     c                   EndIf
077900050429      * incremento il contatore delle note lette
078000050429     c                   Add       1             conta
078100050429     c                   EndDo
078200050429     c                   Select
078300050429      * --> STAMPO ultimo record note
078400050429     c                   When      wnote = *On
078500050517     c                   If        wgiro = 1 and conta > 1
078600050429     c                             and wnote = *On
078700050517     c                   Eval      desnpz = %trim(desnpz)
078800050517     c                   Add       1             curline
078900050517     c                   ExSr      Sr_Over
079000050518     c                   If        wover = *On
079100050518     c                   Write     dn91d19
079200050518     c                   EndIf
079300050429     c                   Write     dn91d8
079400050429     c                   EndIf
079500050517     c                   If        wgiro > 1 and conta > 1
079600050429     c                             and wnote = *On
079700050517     c                   Eval      desnpz1 = %trim(desnpz1)
079800050517     c                   Add       1             curline
079900050517     c                   ExSr      Sr_Over
080000050518     c                   If        wover = *On
080100050518     c                   Write     dn91d19
080200050518     c                   EndIf
080300050429     c                   Write     dn91d9
080400050429     c                   EndIf
080500050429     c                   EndSl
080600050429
080700050429      *
    N.pezzi mancanti/colli/Kg          ?
080800050429     c                   Eval      pdctnpz = dctnpz
080900050429     c                   Eval      pdctncn = dctncn
081000050429     c                   Eval      pdctpkd = dctpkd
081100050429
081200050429      *
    Verifica DDT                       ?
081300050415     c                   If        dcttdt = 'P'
081400050415     c                   Eval      destdt = 'PARTENZA'
081500050415     c                   EndIf
081600050415     c                   If        dcttdt = 'A'
081700050415     c                   Eval      destdt = 'ARRIVO'
081800050415     c                   EndIf
081900050415     c                   If        dcttdt = *Blanks
082000050415     c                   Eval      destdt = 'NESSUNO'
082100050415     c                   EndIf
082200050429
082300050429      *
    Riserva                            ?
082400050415     c                   If        dctcrc  = 'S'
082500050415     c                   Eval      descrc  = 'SI'
082600050415     c                   EndIf
082700050415     c                   If        dctcrc  <> 'S'
082800050415     c                   Eval      descrc  = 'NO'
082900050415     c                   EndIf
083000050429
083100050429      *
    Disposizioni sulle merce danneg.   ?
083200050415     c                   If        dctdsd  = 'C'
083300050415     c                   Eval      desdsd = 'CONSEGNATA'
083400050415     c                   EndIf
083500050415     c                   If        dctdsd  = 'D'
083600050415     c                   Eval      desdsd = 'DISTRUTTA'
083700050415     c                   EndIf
083800050415     c                   If        dctdsd  = 'R'
083900050415     c                   Eval      desdsd = 'RESA'
084000050415     c                   EndIf
084100050415     c                   If        dctdsd  = 'M'
084200050415     c                   Eval      desdsd = 'MAGAZZINO'
084300050415     c                   EndIf
084400050412
084500050429      *
    Beneficiario alternativo           ?
084600050412     c                   If        dctksc > *Zeros
084700050415     c                   Eval      pdctksc = dctksc
084800050412     c                   Move      dctksc        w0040
084900050412      *   se codificato decodifico da p.d.c.
085000050412     c                   If        w0040 <> 8888
085100050412     c                   Clear                   tibs69ds
085200050412     c                   z-add     dctksc        I69kac
085300050412     c                   Call      'TIBS69R'
085400050412     c                   Parm                    tibs69DS
085500050412     c                   Parm                    ds_cnaco
085600050412     c                   Parm                    ds_cnind
085700050412     c                   Parm                    ds_cnclp
085800050412     c                   Parm                    ds_fncls
085900050429     c                   If        o69err <> '1'
086000050412     c                   Eval      desksc = acorag
086100050412     c                   EndIf
086200050412     c                   Else
086300050412      *   se non codificato decodifico da anagrafica clienti c.a.
086400050412     c                   Eval      wtrc = 'C'
086500050412     c     kFndka        Chain     Fndka01l
086600050414     c                   If        %Found(Fndka01l) and dkaatb <> 'A'
086700050412     c                   Eval      desksc = dkarag
086800050412     c                   EndIf
086900050412     c                   EndIf
087000050412     c                   EndIf
087100050415
087200050429      *
    Interlocutore                      ?
087300050415     c                   Eval      kdcstpd = 'C'
087400050415     c                   Movel(p)  dsnumca       kdcsnkt
087500050415     c                   Eval      kdcsstd = 'N'
087600050415     c                   Clear                   kdcsdim
087700050415     c                   Clear                   kdcshim
087800050415     c                   Clear                   kdcsnks
087900050415     c                   Eval      kdcstrc = 'D'
088000050415     c     kFndcs        Chain     Fndcs01l
088100050415     c                   If        %Found(Fndcs01l)
088200050415     c                   Eval      desint = dcsnot
088300050415     c                   EndIf
088400050415
088500050429      *
    Responsabilità internazionale      ?
088600050415     c                   Eval      *In02 = *Off
088700050415     c                   If        dctres = 'V'
088800050415     c                   Eval      desres = 'VETTORE'
088900050415     c                   Eval      *In02 = *On
089000050415     c                   EndIf
089100050415     c                   If        dctres = 'P'
089200050415     c                   Eval      desres = 'PARTNER'
089300050415     c                   Eval      *In02 = *On
089400050415     c                   EndIf
089500050414
089600050429      *
    Rivalsa                            ?
089700050414     c                   Eval      *In01 = *Off
089800050414     c                   Eval      wtrc = 'R'
089900050414     c     kFndka        Chain     Fndka01l
090000050414     c                   If        %Found(Fndka01l) and dkaatb <> 'A'
090100050414     c                   Eval      *In01 = *On
090200050414     c                   Eval      desass = dkarag
090300050414     c                   Eval      codass = dkaksc
090400050414     c                   Eval      viaass = dkavia
090500050414     c                   Eval      capass = dkacap
090600050414     c                   Eval      locass = dkaloc
090700050414     c                   Eval      prvass = dkaprv
090800050414     c                   Eval      nazass = dkanaz
090900050414     c                   Eval      telass = dkatel
091000050414     c                   Eval      faxass = dkafax
091100050414     c                   EndIf
091200050429
091300050429      *
    Causale chiusura C.A.              ?
091400050429     c                   Eval      pdctcch = dctcch
091500050429     c                   Clear                   dcch
091600050429     c                   Clear                   tibs02ds
091700050429     c                   Eval      t02mod = 'C'
091800050429     c                   Eval      t02sif = knsif
091900050429     c                   Eval      t02cod = 'CCH'
092000050429     c                   Eval      t02ke1 = dctcch
092100050429     c                   Call      'TIBS02R'
092200050429     c                   Parm                    kpjba
092300050429     c                   Parm                    tibs02ds
092400050429     c                   If        t02err = *Blanks
092500050429     c                   Eval      dcch = t02uni
092600050429     c                   EndIf
092700050429     c                   Eval      descch = §cchdesc
092800050429
092900050429      *
    Pagamento                          ?
093000050429      * controllo se è stata fatta almeno un'estrazione o il pagamento
093100050429     c                   If        §dctpaga = 'C'
093200050429     c                   Eval      despag = 'Estratta per il pagamento'
093300050429     c                   EndIf
093400050429     c                   If        §dctpaga = 'P'
093500050429     c                   Eval      despag = 'Pagata'
093600050429     c                   EndIf
093700050429
093800050429      *
    Mandato assicurativo               ?
093900050503     c                   Eval      *In05 = *Off
094000050429     c     kFndc         Chain     Fndpt01l
094100050429     c                   If        %Found(Fndpt01l) and dptatb <> 'A'
094200050429     c                   Eval      *In05 = *On
094300050429     c                   Eval      pdptksc = dptksc
094400050429     c                   If        dptftc = *Blanks
094500050429     c                   Eval      desman = 'CLIENTE SENZA MANDATO'
094600050429     c                   Else
094700050429     c                   Clear                   ds1p
094800050429     c                   Eval      ktblcod = '1P'
094900050429     c                   Eval      ktblkey = dptftc
095000050429     c     kTab          Chain     Tabel00f
095100050429     c                   If        %Found(tabel00f)
095200050429     c                   Eval      ds1p = tbluni
095300050429     c                   EndIf
095400050429     c                   Eval      desman = §1pdes
095500050429     c                   EndIf
095600050429      * Ragione sociale cliente
095700050429     c                   Clear                   tibs69ds
095800050429     c                   z-add     dptksc        I69kac
095900050429     c                   Call      'TIBS69R'
096000050429     c                   Parm                    tibs69DS
096100050429     c                   Parm                    ds_cnaco
096200050429     c                   Parm                    ds_cnind
096300050429     c                   Parm                    ds_cnclp
096400050429     c                   Parm                    ds_fncls
096500050429     c                   If        o69err <> '1'
096600050429     c                   Eval      ragdpt = acorag
096700050429     c                   EndIf
096800050429      * codice tariffa
096900050429     c                   Eval      pdptctr = dptctr
097000050429     c                   Clear                   dstr
097100050429     c                   Eval      ktblcod = 'TR'
097200050429     c                   Movel     dptctr        w001a
097300050429     c                   Eval      ktblkey = w001a
097400050429     c     kTab          Chain     Tabel00f
097500050429     c                   If        %Found(tabel00f)
097600050429     c                   Eval      dstr = tbluni
097700050429     c                   EndIf
097800050429     c                   Eval      desctr = §trdes
097900050429      * tipo valore merce
098000050429     c                   Eval      pdptvlm = dptvlm
098100050429     c                   Eval      pdptvvm = dptvvm
098200050429     c                   Clear                   dstr
098300050429     c                   Eval      ktblcod = 'TR'
098400050429     c                   Eval      ktblkey = dptfvm
098500050429     c     kTab          Chain     Tabel00f
098600050429     c                   If        %Found(tabel00f)
098700050429     c                   Eval      dstr = tbluni
098800050429     c                   EndIf
098900050429     c                   Eval      desvvm = §trde3
099000050429     c                   EndIf
099100050429
099200050429      * --> STAMPO 10° record
099300050517     c                   Add       3             curline
099400050517     c   01              Add       1             curline
099500050517     c   05              Add       1             curline
099600050517     c                   ExSr      Sr_Over
099700050518     c                   If        wover = *On
099800050518     c                   Write     dn91d19
099900050518     c                   EndIf
100000050429     c                   Write     dn91d10
100100050415
100200050429      *
    Importo da assicurare              ?
100300050415     c                   Eval      ptasias = tasias
100400050415     c                   Eval      ptasvas = tasvas
100500050415      * se inserito da fatturazione non stampo l'importo da assicurare
100600050415     c                   If        tasfcm = 'F'
100700050415     c                   Eval      *In03 = *Off
100800050415     c                   Else
100900050415     c                   Eval      *In03 = *On
101000050415     c                   EndIf
101100050415
101200050429      *
    Importo Max risarcibile            ?
101300050415     c                   Eval      pdctipv = dctipv
101400050415     c                   Eval      pdctvpv = dctvpv
101500050415      * Tipo inserimento importo max risarcibile
101600050413     c                   If        dctfpv = 'M'
101700050413     c                   Eval      desfpv = 'INSERITO MANUALMENTE'
101800050413     c                   EndIf
101900050413     c                   If        dctfpv = 'C'
102000050413     c                   Eval      desfpv = 'CALCOLATO'
102100050413     c                   EndIf
102200050415
102300050415      * Aggancio file Liquidazione C.A.
102400050415     c     kFndc         Chain     Fndcl01l
102500050415     c                   If        not %Found(Fndcl01l)
102600050418     c                   ExSr      Puldcl
102700050415     c                   EndIf
102800050415
102900050429      *
    Importo richiesto                  ?
103000050415     c                   Eval      pdclipr = dclipr
103100050415     c                   Eval      pdclvpr = dclvpr
103200050413
103300050429      *
    Importo trattenuto                 ?
103400050415     c                   Eval      pdclipt = dclipt
103500050415     c                   Eval      pdclvpt = dclvpt
103600050415      * data importo trattenuto
103700050414     c                   If        dcldpt > *Zeros
103800050414     c     *iso          Move      dcldpt        wdataeur
103900050414     c     *eur          Movel     wdataeur      datapt
104000050414     c                   EndIf
104100050415
104200050429      *
    Importo liquidato                  ?
104300050418      * se è pratica è in un campo... se è liquidazione in un altro
104400050418     c                   If        *In20
104500050418     c                   Eval      pdclipl = dclipl
104600050418     c                   Eval      pdclvpl = dclvpl
104700050418     c                   Else
104800050418     c                   If        *In21
104900050418     c                   Eval      impipp = dclipl
105000050418     c                   Eval      valipp = dclvpl
105100050418     c                   EndIf
105200050418      * se pratica trattenuta da cliente e pagamento già avvenuto imposto come importo
105300050418      * liquidato la differenza
105400050418     c                   If        dclipl <= dclipt
105500050418     c                   Clear                   pdclipl
105600050418     c                   Clear                   pdclvpl
105700050418     c                   Else
105800050418     c                   Eval      pdclipl = dclipl -dclipt
105900050418     c                   Eval      pdclvpl = dclvpl
106000050418     c                   EndIf
106100050418     c                   EndIf
106200160302
106300160302      *
    Assegno/Bonifico                   ?
106400160302     c                   IF        %subst(DCLasn:1:1) = 'B'
106500160302     c                   eval      *in23 = *on
106600160302     c                   ELSE
106700160302     c                   eval      *in23 = *off
106800160302     c                   ENDIF
106900050414
107000050429      *
    Data emissione assegno             ?
107100050414     c                   If        dcldea > *Zeros
107200050414     c     *iso          Move      dcldea        wdataeur
107300050414     c     *eur          Movel     wdataeur      dataea
107400050414     c                   EndIf
107500050414
107600050429      *
    Decodifica ABI/CAB                 ?
107700050414     c                   Move      dclabi        wabi
107800050414     c                   Move      dclcab        wcab
107900050414     c     kCnabi        Chain     Cnabi01l
108000050414     c                   If        %Found (Cnabi01l)
108100050414     c                   Eval      desban = %trim(abiist) + '-' + %trim(abiage)
108200050414     c                             + '-' + %trim(abiloc) + '-' + %trim(abicom)
108300050414     c                   EndIf
108400050418
108500050429      * --> STAMPO 11° record
108600050517     c                   Add       2             curline
108700050517     c                   ExSr      Sr_Over
108800050518     c                   If        wover = *On
108900050518     c                   Write     dn91d19
109000050518     c                   EndIf
109100050429     c                   Write     dn91d11
109200050414
109300050429      *
    Intestatario progetto di liquidaz. ?
109400050418      * Solo se è Pratica Assicurativa o Liquidazione Transattiva
109500050418     c                   If        *In20 or *In21
109600050414     c                   Eval      wtrc = 'I'
109700050414     c     kFndka        Chain     Fndka01l
109800050414     c                   If        %Found(Fndka01l) and dkaatb <> 'A'
109900050414     c                   Eval      despli = dkarag
110000050414     c                   Eval      codpli = dkaksc
110100050414     c                   Eval      viapli = dkavia
110200050429     c                   Eval      locpli = %trim(dkacap) + ' ' + %trim(dkaloc)
110300050429     c                                    + '  ' + dkaprv + ' ' + dkanaz
110400050414     c                   EndIf
110500050414
110600050429      *
    Data stp. progetto di liquidazione ?
110700050414     c                   If        dcldds > *Zeros
110800050414     c     *iso          Move      dcldds        wdataeur
110900050414     c     *eur          Movel     wdataeur      datads
111000050414     c                   EndIf
111100050429
111200050429      *
    Data accett. prg. di liquidazione  ?
111300050414      * controllo la validità della data xchè ce ne sono delle farlocche soprattutto
111400050414      * nella vecchie c.a.
111500050504      * se errata metto data valuta pagamento
111600050414     c                   If        dcldpl > *Zeros
111700050414     c                   clear                   wlbdat
111800050419     c                   Z-add     dcldpl        g02inv
111900050419     c                   Eval      g02err = '3'
112000050414     c                   call      'XSRDA8'
112100050414     c                   parm                    wlbdat
112200050419     c                   If        g02err = '1'
112300050414     c     *iso          Move      dclvlp        wdataeur
112400050414     c     *eur          Movel     wdataeur      datapl
112500050414     c                   Else
112600050510     c                   Move      g02dat        datapl
112700050414     c                   EndIf
112800050414     c                   EndIf
112900050414
113000050429      *
    Data valuta pagamento              ?
113100050504      * controllo la validità della data xchè ce ne sono delle farlocche soprattutto
113200050504      * nella vecchie c.a.
113300050504      * se errata metto data accett. prg. di liquidazione
113400050414     c                   If        dclvlp > *Zeros
113500050504     c                   clear                   wlbdat
113600050510     c                   Z-add     dclvlp        g02inv
113700050504     c                   Eval      g02err = '3'
113800050504     c                   call      'XSRDA8'
113900050504     c                   parm                    wlbdat
114000050504     c                   If        g02err = '1'
114100050504     c     *iso          Move      dcldpl        wdataeur
114200050504     c     *eur          Movel     wdataeur      datalp
114300050504     c                   Else
114400050510     c                   Move      g02dat        datalp
114500050414     c                   EndIf
114600050504     c                   EndIf
114700050418     c                   EndIf
114800050429
114900050429      *
    Note calcolo progetto di liq.      ?
115000050429      *     solo 4 note
115100050503     c                   Eval      *In06 = *Off
115200050429     c                   If        dcldds > *Zeros
115300050429     c                   Eval      kdcstpd = 'C'
115400050429     c                   Movel(p)  dsnumca       kdcsnkt
115500050429     c                   Eval      kdcsstd = 'C'
115600050429     c                   Clear                   kdcsdim
115700050429     c                   Clear                   kdcshim
115800050429     c                   Clear                   kdcsnks
115900050429     c                   Eval      kdcstrc = 'D'
116000050429     c     kFndcs        Setll     Fndcs01l
116100050429     c                   Do        *Hival
116200050429     c     kFndcs        Reade     Fndcs01l
116300050429     c                   If        %Eof(Fndcs01l)
116400050429     c                   Leave
116500050429     c                   EndIf
116600050429     c                   Eval      desplt = %trim(desplt) + ' ' + dcsnot
116700050429     c                   Eval      *In06 = *On
116800050429     c                   EndDo
116900050429     c                   EndIf
117000160302
117100160304      *
    Beneficiario Pagamento             ?
117200160304     c                   Eval      wtrc = 'P'
117300160304     c     kFndka        Chain     Fndka01l
117400160304     c                   If        %Found(Fndka01l) and dkaatb <> 'A'
117500160304     c                   Eval      codben = dkaksc
117600160304     c                   Eval      desben = dkarag
117700160304     c                   Eval      viaben = dkavia
117800160304     c                   Eval      locben = %trim(dkacap) + ' ' + %trim(dkaloc)
117900160304     c                                    + '  ' + dkaprv + ' ' + dkanaz
118000160304     c                   EndIf
118100160304     c                   Write     dn91d20
118200050418
118300050429      * --> STAMPO 12° record
118400050517     c                   Add       2             curline
118500050517     c   06              Add       1             curline
118600050517     c                   ExSr      Sr_Over
118700050518     c                   If        wover = *On
118800050518     c                   Write     dn91d19
118900050518     c                   EndIf
119000050429     c                   Write     dn91d12
119100050414
119200050429      *
    F A S I                            ?
119300050503     c                   Clear                   conta
119400050504     c                   Clear                   s_rigafase
119500050503      * --> STAMPO 13° record
119600050418      *     intestazione fasi
119700050517     c                   Add       3             curline
119800050517     c                   ExSr      Sr_Over
119900050503     c                   Write     dn91d13
120000050418
120100050418      * --> Fasi eseguite
120200050414     c     kFndc         Setll     Fndcf01l
120300050414     c                   Do        *Hival
120400050414     c     kFndc         Reade     Fndcf01l
120500050414     c                   If        %Eof(Fndcf01l)
120600050414     c                   Leave
120700050414     c                   EndIf
120800050414     c                   If        dcfatb = 'A'
120900050414     c                   Iter
121000050414     c                   EndIf
121100050504
121200050504     c                   Add       1             conta
121300050503
121400050503      * Descrizione fase
121500050503     c     dcffca        Chain     Fndfa01l
121600050503     c                   If        Not %Found(Fndfa01l)
121700050503     c                   Clear                   dfades
121800050503     c                   EndIf
121900050503
122000050503     c                   Eval      ddcf01 = dcflet
122100050503
122200050503      * Data esecuzione fase
122300050503     c     *iso          Move      dcfdfc        wdataeur
122400050504     c     *eur          Movel     wdataeur      datafa
122500050504
122600050504      * Imposto i campi come devono essere stampati
122700050504      * data/ora fase/descrizione flag p.o./utente
122800050504     c                   Eval      rigafase1 = %editw(datafa:'  /  /    ') +
122900050504     c                             ' ' + %editw(dcfhfc:'  :  ') + ' ' +
123000050504     c                             %editc(dcffca:'1') + ' ' + dfades + '   ' +
123100050504     c                             §dcfpfra + '    ' + dcfptr + '    ' +
123200050504     c                             %editc(dcffev:'1') + '  ' + dcfpru
123300050504
123400050504     c                   Eval      s_rigafase(conta) = rigafase1
123500050414
123600050414     c                   EndDo
123700050503
123800050504      * Stampo le fasi su due colonne
123900050504     c     conta         div       2             righe
124000050504     c                   mvr                     resto
124100050504     c                   add       resto         righe
124200050504     c                   Clear                   col1
124300050504     c                   Clear                   col2
124400050504     c                   Do        righe         col1
124500050504     c                   Eval      col2 = col1 + righe
124600050504     c                   Clear                   dn91d14
124700050504     c                   Eval      rigafase1 = s_rigafase(col1)
124800050504     c                   Eval      rigafase2 = s_rigafase(col2)
124900050503      * --> STAMPO 14° record
125000050517     c                   Add       1             curline
125100050517     c                   ExSr      Sr_Over
125200050518     c                   If        wover = *On
125300050518     c                   Write     dn91d13
125400050518     c                   EndIf
125500050503     c                   Write     dn91d14
125600050504     c                   EndDo
125700050414
125800050503      *
    Documenti Richiesti                ?
125900050509     c                   Clear                   conta
126000050509     c                   Clear                   s_rigadoc
126100050509     c     kFndc         Setll     Fndcc01l
126200050509     c                   If        %Equal(Fndcc01l)
126300050509      * --> STAMPO 15° record
126400050509      *     intestazione documenti
126500050517     c                   Add       3             curline
126600050517     c                   ExSr      Sr_Over
126700050509     c                   Write     dn91d15
126800050509
126900050509     c                   Do        *Hival
127000050509     c     kFndc         Reade     Fndcc01l
127100050509     c                   If        %Eof(Fndcc01l)
127200050509     c                   Leave
127300050509     c                   EndIf
127400050509     c                   If        dccatb = 'A'
127500050509     c                   Iter
127600050509     c                   EndIf
127700050509
127800050509     c                   Add       1             conta
127900050509
128000050509      * Descrizione tipo documento
128100050509     c                   Clear                   dcdd
128200050509     c                   Clear                   tibs02ds
128300050509     c                   Eval      t02mod = 'C'
128400050509     c                   Eval      t02sif = knsif
128500050509     c                   Eval      t02cod = 'CDD'
128600050509     c                   Eval      t02ke1 = dcccdd
128700050509     c                   Call      'TIBS02R'
128800050509     c                   Parm                    kpjba
128900050509     c                   Parm                    tibs02ds
129000050509     c                   If        t02err = *Blanks
129100050509     c                   Eval      dcdd = t02uni
129200050509     c                   EndIf
129300050509
129400050510     c                   Clear                   datarz
129500050510     c                   Clear                   datadc
129600050509      * Data ricezione documento
129700050510      * controllo la validità della data xchè ce ne sono delle farlocche soprattutto
129800050510      * nella vecchie c.a.
129900050510      * se errata la azzero e non la stampo
130000050509     c                   If        dccdrz > *Zeros
130100050510     c                   clear                   wlbdat
130200050510     c                   Z-add     dccdrz        g02inv
130300050510     c                   Eval      g02err = '3'
130400050510     c                   call      'XSRDA8'
130500050510     c                   parm                    wlbdat
130600050510     c                   If        g02err = '1'
130700050510     c                   Clear                   datarz
130800050510     c                   Else
130900050510     c                   Move      g02dat        datarz
131000050510     c                   EndIf
131100050509     c                   EndIf
131200050509
131300050509      * Imposto i campi come devono essere stampati
131400050509      * data/tipo doc/descrizione
131500050509     c                   Eval      rigadoc1 = %editw(datarz:'  /  /    ') +
131600050509     c                             ' ' + dcccdd + ' ' + §cdddesc
131700050510     c                   Eval      s_rigadoc(conta) = rigadoc1
131800050509
131900050509      * Cerco i dati previsti per il documento
132000050509     c                   If        §cddpnum = 'S'
132100050509      * e se è già pervenuto altrimenti non faccio vedere i campi vuoti
132200050509     c                             and dccdrz > 0
132300050510
132400050510     c                   Add       1             conta
132500050509      * data documento
132600050509     c                   If        dclddc > *Zeros
132700050509     c     *iso          Move      dclddc        wdataeur
132800050509     c     *eur          Movel     wdataeur      datadc
132900050510     c                   EndIf
133000050509      * Imposto i campi come devono essere stampati
133100050509      * numero doc./data/importo/valuta
133200050510     c                   Eval      rigadoc1 = w014a + 'Numero:' + ' ' +
133300050509     c                                      %trim(%editc(dclndc:'4')) + ' ' +
133400050509     c                                     'Data documento:' + ' ' +
133500050509     c                                      %trim(%editw(datadc:'  /  /    ')) +
133600050509     c                                     ' ' + 'Importo:' + ' ' +
133700050509     c                                      %trim(%editc(dclipr:'4')) + ' ' +
133800050509     c                                      dclvpr
133900050510     c                   Eval      s_rigadoc(conta) = rigadoc1
134000050509     c                   EndIf
134100050509
134200050509     c                   EndDo
134300050509
134400050509      * Stampo i documenti su due colonne
134500050509     c     conta         div       2             righe
134600050509     c                   mvr                     resto
134700050509     c                   add       resto         righe
134800050509     c                   Clear                   col1
134900050509     c                   Clear                   col2
135000050509     c                   Do        righe         col1
135100050509     c                   Eval      col2 = col1 + righe
135200050509     c                   Clear                   dn91d16
135300050509     c                   Eval      rigadoc1 = s_rigadoc(col1)
135400050509     c                   Eval      rigadoc2 = s_rigadoc(col2)
135500050509      * --> STAMPO 16° record
135600050517     c                   Add       1             curline
135700050517     c                   ExSr      Sr_Over
135800050518     c                   If        wover = *On
135900050518     c                   Write     dn91d15
136000050518     c                   EndIf
136100050509     c                   Write     dn91d16
136200050509     c                   EndDo
136300050509     c                   EndIf
136400050516
136500050516      *
    Note generiche                     ?
136600050516     c                   Eval      kdcstpd = 'C'
136700050516     c                   Movel(p)  dsnumca       kdcsnkt
136800050516     c                   Clear                   kdcsstd
136900050517     c                   Eval      conta = 1
137000050517     c                   Eval      wgiro = 1
137100050517     c                   Clear                   oldhim
137200050516     c     kFndcs02      Setll     Fndcs01l
137300050516     c                   If        %Equal(Fndcs01l)
137400050516      * --> STAMPO 17° record
137500050516      *     intestazione note
137600050517     c                   Add       3             curline
137700050517     c                   ExSr      Sr_Over
137800050516     c                   Write     dn91d17
137900050516
138000050516     c                   Do        *Hival
138100050516     c     kFndcs02      Reade     Fndcs01l
138200050516     c                   If        %Eof(Fndcs01l)
138300050516     c                   Leave
138400050516     c                   EndIf
138500050516     c                   If        dcsatb = 'A'
138600050516     c                   Iter
138700050516     c                   EndIf
138800050517
138900050517     c                   Eval      *In07 = *Off
139000050516
139100050517      * se stessa data/ora immissione ed ho già caricato 4 note nel campo nota
139200050517      * stampo
139300050517     c                   If        conta = 5 and
139400050517     c                             (oldhim = dcshim and oldhim > *Zeros)
139500050517      * se non è la prima volta che stampo non stampo la data/ora immissione e la fase
139600050517     c                   If        wgiro > 1
139700050517     c                   Eval      *In07 = *On
139800050517     c                   EndIf
139900050517      * se è la prima volta che stampo devo stampare la data/ora immissione e la fase
140000050517     c                   If        wgiro = 1
140100050517     c                   Add       1             wgiro
140200050517     c                   EndIf
140300050517     c                   Eval      riganota = %trim(riganota)
140400050517      * --> STAMPO 18° record
140500050517     c                   Add       1             curline
140600050517     c                   ExSr      Sr_Over
140700050518     c                   If        wover = *On
140800050518     c                   Write     dn91d17
140900050518     c                   EndIf
141000050517     c                   Write     dn91d18
141100050517     c                   Clear                   riganota
141200050517     c                   Eval      conta = 1
141300050517     c                   EndIf
141400050517
141500050517      * a rottura di ora immissione stampo le note
141600050517     c                   If        oldhim <> dcshim
141700050517     c                   If        oldhim > *Zeros
141800050517      * se non è la prima volta che stampo non stampo la data/ora immissione e la fase
141900050517     c                   If        wgiro > 1
142000050517     c                   Eval      *In07 = *On
142100050517     c                   EndIf
142200050517     c                   Eval      riganota = %trim(riganota)
142300050517      * --> STAMPO 18° record
142400050517     c                   Add       1             curline
142500050517     c                   ExSr      Sr_Over
142600050518     c                   If        wover = *On
142700050518     c                   Write     dn91d17
142800050518     c                   EndIf
142900050517     c                   Write     dn91d18
143000050517     c                   Clear                   riganota
143100050517     c                   Eval      conta = 1
143200050517     c                   Eval      wgiro = 1
143300050517     c                   EndIf
143400050517     c                   Eval      oldhim = dcshim
143500050517     c                   EndIf
143600050516
143700050516     c                   Clear                   datanot
143800050516      * Data nota
143900050516     c     *iso          Move      dcsdim        wdataeur
144000050516     c     *eur          Movel     wdataeur      datanot
144100050516
144200050516      * Fase nota
144300050516     c                   Eval      pfaseno = %subst(dcsnks:1:3)
144400050517      * P.o. nota e utente
144500050517     c                   Eval      pdcspos = dcspos
144600050517     c                   Eval      pdcspru = dcspru
144700050516
144800050516      * Imposto la riga della nota 4 note x riga
144900050517     c                   Eval      riganota = %trim(riganota) + ' ' + dcsnot
145000050517     c                   Add       1             conta
145100050516
145200050516     c                   EndDo
145300050517
145400050517      * se a fine lettura ho ancora delle note da stampare le stampo
145500050517     c                   If        conta > 1
145600050517     c                   If        wgiro > 1
145700050517     c                   Eval      *In07 = *On
145800050517     c                   EndIf
145900050517     c                   Eval      riganota = %trim(riganota)
146000050517     c                   Add       1             curline
146100050517     c                   ExSr      Sr_Over
146200050518     c                   If        wover = *On
146300050518     c                   Write     dn91d17
146400050518     c                   EndIf
146500050517     c                   Write     dn91d18
146600050517     c                   EndIf
146700050516
146800050516     c                   EndIf
146900050408
147000050411     c                   EndSr
147100050517
147200050517      *------------------------------------------------------------------------*
147300050517      * GESTIONE SALTO PAGINA
147400050517      *------------------------------------------------------------------------*
147500050517     c     Sr_Over       BegSr
147600050517
147700050518     c                   Eval      wover = *Off
147800050517     c                   If        *In98 or curline >= 63
147900050517     c                   Write     dn91t1
148000050517     c                   Write     dn91d1
148100050517     c                   Eval      *In98 = *Off
148200050518     c                   Eval      wover = *On
148300050517     c                   EndIf
148400050517
148500050517     c                   EndSr
148600050414
148700050414      *------------------------------------------------------------------------*
148800050414      *  PULIZIA CAMPI
148900050414      *------------------------------------------------------------------------*
149000050414     c     Pulisci       BegSr
149100050414
149200050415     c                   Clear                   dn91d1
149300050415     c                   Clear                   dn91d2
149400050415     c                   Clear                   dn91d3
149500050415     c                   Clear                   dn91d4
149600050415     c                   Clear                   dn91d5
149700050415     c                   Clear                   dn91d6
149800050418     c                   Clear                   dn91d7
149900050418     c                   Clear                   dn91d8
150000050418     c                   Clear                   dn91d9
150100050418     c                   Clear                   dn91d10
150200050418     c                   Clear                   dn91d11
150300050418     c                   Clear                   dn91d12
150400050418     c                   Clear                   dn91d13
150500050418     c                   Clear                   dn91d14
150600050504     c                   Clear                   dn91d15
150700050504     c                   Clear                   dn91d16
150800050516     c                   Clear                   dn91d17
150900050516     c                   Clear                   dn91d18
151000160304     c                   Clear                   dn91d20
151100050414
151200050414     c                   EndSr
151300050414
151400050418      *------------------------------------------------------------------------*
151500050418      *  PULIZIA CAMPI FILE FNDCL
151600050418      *------------------------------------------------------------------------*
151700050418     c     Puldcl        BegSr
151800050418
151900050418     c                   Clear                   dclipr
152000050418     c                   Clear                   dclvpr
152100050418     c                   Clear                   dclipt
152200050418     c                   Clear                   dclvpt
152300050418     c                   Clear                   dcldpt
152400050418     c                   Clear                   dclipl
152500050418     c                   Clear                   dclvpl
152600050418     c                   Clear                   dclndc
152700050418     c                   Clear                   dclddc
152800050418     c                   Clear                   dclvlp
152900050418     c                   Clear                   dcldpl
153000050418     c                   Clear                   dcldds
153100050418     c                   Clear                   dclabi
153200050418     c                   Clear                   dclcab
153300050418     c                   Clear                   dclasn
153400050418     c                   Clear                   dcldea
153500050418
153600050418     c                   EndSr
153700050418
153800050411      *------------------------------------------------------------------------*
153900050411      *  CONTROLLO TIPO BOLLA
154000050411      *------------------------------------------------------------------------*
154100050411     c     Ctrtbl        BegSr
154200050411
154300050411     c                   Clear                   dstb
154400050412     c                   Eval      ktblcod = 'TB'
154500050412     c                   Eval      ktblkey = tastbl
154600050411     c     kTab          Chain     Tabel00f
154700050411     c                   If        %Found(tabel00f)
154800050411     c                   Eval      dstb = tbluni
154900050411     c                   If        §tbrbl <> 'R'
155000050411     c                   Eval      flgtb1 = §tbtpo
155100050411     c                   EndIf
155200050411     c                   EndIf
155300050411
155400050411     c                   EndSr
155500050414
155600050411      *------------------------------------------------------------------------*
155700050411      * ROUTINE INIZIALE
155800050411      *------------------------------------------------------------------------*
155900050411     c     *Inzsr        BegSr
156000050411
156100050411     c     *Entry        Plist
156200050411     c                   Parm                    kpjba
156300050411     c                   Movel     kpjbu         parm01
156400050414
156500050418     c                   If        wflg = *Blanks
156600050418     c                   Eval      *In10 = *Off
156700050418     c                   Clear                   waac
156800050418     c                   Clear                   wfil
156900050418     c                   Clear                   wnca
157000050418     c                   Else
157100050418     c                   Eval      *In10 = *On
157200050418     c                   EndIf
157300050411
157400050411     c     *dtaara       define    §azute        azuteds
157500050411     c     *dtaara       define    §datiute      ddatiute
157600050411
157700050411     c                   in(E)     *dtaara
157800050411     c                   IF        %ERROR or RSUT = *blanks
157900050411     c                   clear                   Tibs34Ds
158000050411     c                   call      'TIBS34R'
158100050411     c                   parm                    Tibs34Ds
158200050411     c                   in        *dtaara
158300050411     c                   ENDIF
158400050412
158500050412     c                   Z-add     *date         wdata
158600050414
158700050414     c     kwndc         klist
158800050414     c                   kfld                    waac
158900050414     c                   kfld                    wfil
159000050414     c                   kfld                    wnca
159100050411
159200050411     c     kTitas        Klist
159300050411     c                   Kfld                    dctaas
159400050411     c                   Kfld                    dctlnp
159500050411     c                   Kfld                    dctnrs
159600050411     c                   Kfld                    dctnsp
159700050503
159800050503     c     kTita4        Klist
159900050503     c                   Kfld                    dctaas
160000050503     c                   Kfld                    dctlnp
160100050503     c                   Kfld                    dctnrs
160200050503     c                   Kfld                    dctnsp
160300050503     c                   kfld                    ktrc
160400050411
160500050411     c     kTab          klist
160600050411     c                   kfld                    codut
160700050412     c                   kfld                    ktblcod
160800050412     c                   kfld                    ktblkey
160900050412
161000050412     c     kFndcs        klist
161100050412     c                   kfld                    kdcstpd
161200050412     c                   kfld                    kdcsnkt
161300050412     c                   kfld                    kdcsstd
161400050412     c                   kfld                    kdcsdim
161500050412     c                   kfld                    kdcshim
161600050412     c                   kfld                    kdcsnks
161700050412     c                   kfld                    kdcstrc
161800050415
161900050415     c     kFndcs01      klist
162000050415     c                   kfld                    kdcstpd
162100050415     c                   kfld                    kdcsnkt
162200050415     c                   kfld                    kdcsstd
162300050415     c                   kfld                    kdcsdim
162400050415     c                   kfld                    kdcshim
162500050415     c                   kfld                    kdcsnks
162600050415     c                   kfld                    kdcstrc
162700050415     c                   kfld                    kdcspno
162800050516
162900050516     c     kFndcs02      klist
163000050516     c                   kfld                    kdcstpd
163100050516     c                   kfld                    kdcsnkt
163200050516     c                   kfld                    kdcsstd
163300050412
163400050412     c     kFndet        klist
163500050412     c                   kfld                    dctaae
163600050412     c                   kfld                    dctnev
163700050503
163800050503     c     kFndet2       klist
163900050503     c                   kfld                    dctaad
164000050503     c                   kfld                    dctpod
164100050503     c                   kfld                    dctnde
164200050412
164300050412     c     kFndka        klist
164400050412     c                   kfld                    dctaac
164500050412     c                   kfld                    dctfil
164600050412     c                   kfld                    dctnca
164700050412     c                   kfld                    wtrc
164800050411
164900050414     c     kFndc         klist
165000050413     c                   kfld                    dctaac
165100050413     c                   kfld                    dctfil
165200050413     c                   kfld                    dctnca
165300050414
165400050414     c     kCnabi        klist
165500050414     c                   kfld                    wabi
165600050414     c                   kfld                    wcab
165700050413
165800050411     c                   EndSr
