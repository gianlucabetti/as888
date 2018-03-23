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
004100050413      * 98   - Overflow
004200050412      * 99   - comodo
004300050504
004400050504      *------------------------------------------------------------------------*
004500050504      *   S C H I E R E
004600050504      *------------------------------------------------------------------------*
004700050509     d s_rigadoc       s             74    dim(500)
004800050516     d s_rigafase      s             97    dim(500)
004900050408
005000050408      *------------------------------------------------------------------------*
005100050408      *   V A R I A B I L I
005200050408      *------------------------------------------------------------------------*
005300050411     d codut           s              1  0 inz(1)
005400050504     d col1            s              3  0
005500050504     d col2            s              3  0
005600050412     d conta           s              3  0
005700050503     d datadc          s                   like(dclddc)
005800050504     d datafa          s                   like(dclddc)
005900050516     d datanot         s                   like(dcsdim)
006000050509     d datarz          s                   like(dclddc)
006100050411     d flgtb1          s                   like(§tbtpo)
006200050412     d kdcsdim         s                   like(DcsDim)
006300050412     d kdcshim         s                   like(DcsHim)
006400050412     d kdcsnks         s                   like(DcsNks)
006500050412     d kdcsnkt         s                   like(DcsNkt)
006600050414     d kdcspno         s                   like(DcsPno)
006700050412     d kdcsstd         s                   like(DcsStd)
006800050412     d kdcstpd         s                   like(DcsTpd)
006900050412     d kdcstrc         s                   like(DcsTrc)
007000050412     d ktblcod         s                   like(TblCod)
007100050412     d ktblkey         s                   like(TblKey)
007200050428     d ktrc            s                   like(ta4trc)
007300050517     d oldhim          s                   like(dcshim)
007400050504     d resto           s              3  0
007500050504     d righe           s              3  0
007600050414     d wabi            s                   like(AbiAbi)
007700050414     d wcab            s                   like(AbiCab)
007800050414     d wdataeur        s               d   datfmt(*eur)
007900050412     d wdcsnot         s                   like(DcsNot)
008000050503     d wdoc            s              1    inz('0')
008100050415     d wgiro           s              3  0 inz
008200050415     d wnote           s              1    inz('0')
008300050518     d wover           s              1    inz('0')
008400050412     d wtrc            s                   like(DkaTrc)
008500050419     d w001a           s              1
008600050412     d w0040           s              4  0
008700050412     d w0080           s              8  0
008800050510     d w014a           s             14
008900050408
009000050408      *------------------------------------------------------------------------*
009100050408      *   D S   I N T E R N E / E S T E R N E
009200050408      *------------------------------------------------------------------------*
009300050408     d parm01          ds
009400050516     d  waaca                         4  0
009500050418     d  wflg                          1
009600050414     d  waac                          4  0
009700050414     d  wfil                          3  0
009800050414     d  wnca                          7  0
009900050413
010000050413     d prtline         ds
010100050413     d  curline              367    368i 0
010200050412
010300050412     d                 ds
010400050412     d  dctaac                 1      4  0
010500050412     d  dctfil                 5      7  0
010600050412     d  dctnca                 8     14  0
010700050412     d  dsnumca                1     14  0
010800050414
010900050428     d                 ds
011000050503     d  wdctaac                1      4  0
011100050428     d  dctmgc                 5      8  0
011200050428     d  wdataca                1      8  0
011300050428
011400050414     d wlbdat          ds                  inz
011500050414     d  g02dat                 1      8  0
011600050414     d  g02inv                 9     16  0
011700050414     d  g02err                17     17
011800050414     d  g02tgi                18     22  0
011900050408
012000050412     d azuteds       e ds                  extname(AZUTE00F)
012100050412     d dcch          e ds
012200050414     d dcdd          e ds
012300050412     d ddatiute      e ds
012400050414     d ddcf01        e ds
012500050411     d ddct01        e ds
012600050411     d dstb          e ds
012700050414     d dstr          e ds
012800050414     d ds1p          e ds
012900050411     d dtad          e ds
013000050428     d dta4a         e ds
013100050412     d ds_cnaco      e ds                  extname(CNACO00F)
013200050412     d ds_cnind      e ds                  extname(CNIND00F)
013300050412     d ds_cnclp      e ds                  extname(CNCLP00F)
013400050412     d ds_fncls      e ds                  extname(FNCLS00F)
013500050408     d kpjba         e ds
013600050412     d tibs02ds      e ds
013700050412     d tibs34ds      e ds                  inz
013800050412     d tibs69ds      e ds
013900050408
014000050408      *------------------------------------------------------------------------*
014100050414
014200050414     c   10kwndc         Setll     Fndct01l
014300050516     c  n10waaca         Setll     Fndct01l
014400050411     c                   Do        *Hival
014500050414     c   10kwndc         Reade     Fndct01l
014600050516     c  n10waaca         Reade     Fndct01l
014700050415
014800050411      * Fine file
014900050516     c                   If        %Eof(Fndct01l)
015000050411     c                   Leave
015100050411     c                   EndIf
015200050503
015300050503     c                   Eval      wdctaac = dctaac
015400050415
015500050415      * Pulizia dei record di stampa
015600050415     c                   ExSr      Pulisci
015700050415
015800050415      * Valorizzo i flag operativi
015900050415     c                   Eval      ddct01 = dctflo
016000050412
016100050415      * Stampo una c.a. x pagina
016200050606     c                   Clear                   page
016300050517     c                   Add       1             curline
016400050413     c                   Write     dn91t1
016500050411
016600050415      * Recupero i dati e stampo il dettaglio della c.a.
016700050411     c                   ExSr      Elabora
016800050411
016900050411     c                   EndDo
017000050411
017100050411     c                   Eval      *InLr = *On
017200050414
017300050411      *------------------------------------------------------------------------*
017400050411      * ELABORO LA C.A.
017500050411      *------------------------------------------------------------------------*
017600050411     c     Elabora       BegSr
017700050415
017800050429      *
    C.A.                               ?
017900050415     c                   Eval      pdctfil = dctfil
018000050415     c                   Eval      pdctnca = dctnca
018100050415      * Data della c.a.
018200050412     c                   Move      dctmgc        w0080
018300050412     c                   Movel     dctaac        w0080
018400050414     c     *iso          Move      w0080         wdataeur
018500050414     c     *eur          Movel     wdataeur      dataca
018600050415
018700050429      *
    Pratica assicurativa               ?
018800050415     c                   Eval      pdctdit = dctdit
018900050415     c                   Eval      pdctpra = dctpra
019000050415     c                   Eval      pdctprn = dctprn
019100050429
019200050429      *
    Tipo gestione                      ?
019300050429     c                   Eval      *In20 = *Off
019400050429     c                   Eval      *In21 = *Off
019500050429     c                   Eval      *In22 = *Off
019600050429     c                   Select
019700050429     c                   When      dctfpr = 'T'
019800050429     c                   Eval      desfpr = 'LIQUIDAZIONE TRANSATTIVA'
019900050429     c                   Eval      tdespag = 'ESTREMI PAGAMENTO L.T.:'
020000050429     c                   Eval      *In20 = *On
020100050429     c                   When      dctfpr = 'P' and §dctpfra <> 'F'
020200050429     c                   Eval      desfpr = 'PRATICA ASSICURATIVA'
020300050429     c                   Eval      tdespag = 'ESTREMI PAGAMENTO PRATICA:'
020400050429     c                   Eval      *In21 = *On
020500050429     c                   When      dctfpr = 'P' and §dctpfra = 'F'
020600050429     c                   Eval      desfpr = 'PRATICA ASSICURAT.IN FRANCHIGIA'
020700050429     c                   Eval      tdespag = 'ESTREMI PAGAMENTO PRATICA IN FRAN-
020800050429     C                             CHIGIA:'
020900050429     c                   Eval      *In22 = *On
021000050429     c                   Other
021100050429     c                   Eval      tdespag = 'ESTREMI PAGAMENTO:'
021200050429     c                   EndSl
021300050429      * Gestione con Mittente / Destinatario
021400050429     c                   If        dctptr = 'M'
021500050429     c                   Eval      desfpr = %trim(desfpr) + ' con MITTENTE'
021600050429     c                   EndIf
021700050429     c                   If        dctptr = 'D'
021800050429     c                   Eval      desfpr = %trim(desfpr) + ' con DESTINATARIO'
021900050429     c                   EndIf
022000050415
022100050429      *
    Denuncia                           ?
022200050415     c                   Eval      pdctaad = dctaad
022300050415     c                   Eval      pdctpod = dctpod
022400050415     c                   Eval      pdctnde = dctnde
022500050428      * cerco la data di stampa della denuncia
022600050428     c                   If        dctnde > *Zeros
022700050428     c     kFndet2       Chain     Fndet02l
022800050428     c                   If        %Found(Fndet02l)
022900050428      * data di stampa della denuncia
023000050428     c                   Move      detdsb        w0080
023100050428     c     *iso          Move      w0080         wdataeur
023200050428     c     *eur          Movel     wdataeur      datade
023300050428     c                   EndIf
023400050428     c                   EndIf
023500050415
023600050428     c                   Eval      *In04 = *Off
023700050428
023800050429      *
    Evento                             ?
023900050415     c                   If        dctnev > *Zeros
024000050428     c                   Eval      *In04 = *On
024100050415     c                   Eval      pdctaae = dctaae
024200050415     c                   Eval      pdctnev = dctnev
024300050415      * reperisco i dati relativi all'evento
024400050415     c     kFndet        Chain     Fndet01l
024500050415     c                   If        %Found(Fndet01l)
024600050415      * data dell'evento
024700050415     c                   Move      detmge        w0080
024800050415     c                   Movel     detaae        w0080
024900050415     c     *iso          Move      w0080         wdataeur
025000050415     c     *eur          Movel     wdataeur      dataev
025100050415      * tipo anomalia evento
025200050415     c                   Clear                   dtad
025300050415     c                   Clear                   tibs02ds
025400050415     c                   Eval      t02mod = 'C'
025500050415     c                   Eval      t02sif = knsif
025600050415     c                   Eval      t02cod = 'TAD'
025700050415     c                   Eval      t02ke1 = dettad
025800050415     c                   Call      'TIBS02R'
025900050415     c                   Parm                    kpjba
026000050415     c                   Parm                    tibs02ds
026100050415     c                   If        t02err = *Blanks
026200050415     c                   Eval      dtad = t02uni
026300050415     c                   EndIf
026400050415     c                   Eval      destev = §taddesc
026500050415      * descrizione aggiuntiva evento
026600050415     c                   Eval      kdcstpd = 'C'
026700050415     c                   Movel(p)  dsnumca       kdcsnkt
026800050415     c                   Eval      kdcsstd = 'E'
026900050415     c                   Clear                   kdcsdim
027000050415     c                   Clear                   kdcshim
027100050415     c                   Clear                   kdcsnks
027200050415     c                   Eval      kdcstrc = 'D'
027300050415     c     kFndcs        Chain     Fndcs01l
027400050415     c                   If        %Found(Fndcs01l)
027500050415     c                   Eval      deseva = dcsnot
027600050415     c                   EndIf
027700050415     c                   EndIf
027800050415     c                   EndIf
027900050415
028000050429      *
    Spedizione                         ?
028100050415     c     kTitas        Chain     Tntmd01l
028200090122     c                   if        not %found(tntmd01l)
028300090122     c                   clear     *all          tntmd000
028400090122     c                   endif
028500050415     c     kTitas        Chain     Titas30c
028600090122     c                   if        not %found(titas30c)
028700090122     c                   clear     *all          titas000
028800090122     c                   endif
028900050415     c                   ExSr      Ctrtbl
029000050415     c                   If        flgtb1 = *Blanks
029100050415     c     kTitas        Reade     Titas30c
029200050415     c                   EndIf
029300050415      * mittente/destinario
029400050415     c                   If        §dctport = 'A'
029500050415     c                   Eval      climit = tasccm
029600050415     c                   Eval      clides = tasksc
029700050415     c                   Else
029800050415     c                   Eval      climit = tasksc
029900050415     c                   Clear                   clides
030000050415     c                   EndIf
030100050415      * indirizzo mittente
030200050415     c                   Eval      ptmdrsm = tmdrsm
030300050516     c                   Eval      deslom = %trim(tmdinm) + ' ' + '-' + ' ' +
030400050516     c                                      %trim(tmdcam) + ' ' + %trim(tmdlom)
030500050516     c                                      + '  ' + tmdprm + ' ' + tmdnzm
030600050415      * indirizzo destinatario
030700050415     c                   Eval      ptmdrsd = tmdrsd
030800050516     c                   Eval      deslod = %trim(tmdind) + ' ' + '-' + ' ' +
030900050516     c                                      %trim(tmdcad) + ' ' + %trim(tmdlod)
031000050516     c                                      + '  ' + tmdprd + ' ' + tmdnzd
031100050415      * numero spedizone
031200050415     c                   Eval      pdctlnp = dctlnp
031300050415     c                   Eval      pdctnrs = dctnrs
031400050415     c                   Eval      pdctnsp = dctnsp
031500050415      * data della spedizione
031600050415     c                   Move      tasmgs        w0080
031700050415     c                   Movel     tasaas        w0080
031800090331      * se data a 0 pulisco il campo data spedizione
031900090331     c                   if        w0080 = *zeros
032000090331     c                   clear                   datasp
032100090331     c                   else
032200050415     c     *iso          Move      w0080         wdataeur
032300050415     c     *eur          Movel     wdataeur      datasp
032400090331     c                   endif
032500050415      * tipo bolla assegnato/franco
032600050411     c                   If        §dctport = 'F'
032700050411     c                   Eval      descbo = 'FRANCO'
032800050411     c                   Else
032900050411     c                   Eval      descbo = 'ASSEGNATO'
033000050411     c                   EndIf
033100050415      * linea di arrivo
033200050415     c                   Eval      pdctlna = dctlna
033300050415      * riferimento mittente
033400050415     c                   Eval      ptasrmn = tasrmn
033500050428      * riferimento mittente alfanumerico
033600050428     c                   Eval      ktrc = 'A'
033700050428     c     ktita4        Chain     Tita430c
033800050428     c                   If        %Found(Tita430c)
033900050428     c                   Eval      dta4a = ta4not
034000050428     c                   Eval      pta4rma = §ta4arma
034100050428     c                   EndIf
034200050415      * colli/peso/volume
034300050415     c                   Eval      ptasncl = tasncl
034400050415     c                   Eval      ptaspkf = taspkf
034500050415     c                   Eval      ptasfvf = tasfvf
034600050415     c                   Eval      ptasvlf = tasvlf
034700050415      * natura merce
034800050415     c                   Eval      pdctnas = dctnas
034900050415
035000050415      * --> STAMPO 1° record
035100050517     c  n04              Add       5             curline
035200050517     c   04              Add       6             curline
035300050415     c                   Write     dn91d1
035400050412
035500050429      *
    Anomalia                           ?
035600050418     c                   Eval      pdcttad = dcttad
035700050415      * decodifico
035800050412     c                   Clear                   dtad
035900050412     c                   Clear                   tibs02ds
036000050412     c                   Eval      t02mod = 'C'
036100050412     c                   Eval      t02sif = knsif
036200050412     c                   Eval      t02cod = 'TAD'
036300050412     c                   Eval      t02ke1 = dcttad
036400050412     c                   Call      'TIBS02R'
036500050412     c                   Parm                    kpjba
036600050412     c                   Parm                    tibs02ds
036700050412     c                   If        t02err = *Blanks
036800050412     c                   Eval      dtad = t02uni
036900050412     c                   EndIf
037000050415     c                   Eval      destad = §taddesc
037100050415     c                   Eval      wnote = *Off
037200050415      * cerco le descrizioni aggiuntive
037300050412     c                   Eval      kdcstpd = 'C'
037400050412     c                   Movel(p)  dsnumca       kdcsnkt
037500050412     c                   Eval      kdcsstd = 'A'
037600050412     c                   Clear                   kdcsdim
037700050412     c                   Clear                   kdcshim
037800050412     c                   Clear                   kdcsnks
037900050412     c                   Eval      kdcstrc = 'D'
038000050415     c                   Eval      conta = 1
038100050415     c                   Eval      wgiro = 1
038200050415     c     kFndcs        Setll     Fndcs01l
038300050415     c                   Do        *Hival
038400050415     c     kFndcs        Reade     Fndcs01l
038500050415     c                   If        %Eof(Fndcs01l)
038600050415     c                   Leave
038700050415     c                   EndIf
038800050415     c                   Eval      wnote = *On
038900050415     c                   Select
039000050415      * se primo giro devo stampare 4 note assieme
039100050415     c                   When      wgiro = 1
039200050415      * se ho già caricato 4 note salvo la chiave della nota che sto leggendo x poi riposizionarmi
039300050415     c                   If        conta = 5
039400050415     c                   Eval      kdcstpd = dcstpd
039500050415     c                   Eval      kdcsnkt = dcsnkt
039600050415     c                   Eval      kdcsstd = dcsstd
039700050415     c                   Eval      kdcsdim = dcsdim
039800050415     c                   Eval      kdcshim = dcshim
039900050415     c                   Eval      kdcsnks = dcsnks
040000050415     c                   Eval      kdcstrc = dcstrc
040100050415     c                   Eval      kdcspno = dcspno
040200050415      * --> STAMPO 2° record
040300050517     c                   Eval      destag = %trim(destag)
040400050517     c                   Add       1             curline
040500050415     c                   Write     dn91d2
040600050415      * riporto il contatore a 1
040700050415     c                   Eval      conta = 1
040800050415     c                   Eval      wgiro = 2
040900050415      * mi riposiziono sul file delle note all'ultima nota letta e torno a leggere
041000050415     c     kFndcs01      Setll     Fndcs01l
041100050415     c                   Iter
041200050415     c                   EndIf
041300050428      * se non è il primo giro devo stampare 4 note assieme
041400050415     c                   Other
041500050428      * se ho già caricato 4 note salvo la chiave della nota che sto leggendo x poi riposizionarmi
041600050428     c                   If        conta = 5
041700050415     c                   Eval      kdcstpd = dcstpd
041800050415     c                   Eval      kdcsnkt = dcsnkt
041900050415     c                   Eval      kdcsstd = dcsstd
042000050415     c                   Eval      kdcsdim = dcsdim
042100050415     c                   Eval      kdcshim = dcshim
042200050415     c                   Eval      kdcsnks = dcsnks
042300050415     c                   Eval      kdcstrc = dcstrc
042400050415     c                   Eval      kdcspno = dcspno
042500050415      * --> STAMPO 3° record
042600050517     c                   Eval      desta1 = %trim(desta1)
042700050517     c                   Add       1             curline
042800050517     c                   ExSr      Sr_Over
042900050518     c                   If        wover = *On
043000050518     c                   Write     dn91d19
043100050518     c                   EndIf
043200050415     c                   Write     dn91d3
043300050418     c                   Clear                   desta1
043400050415      * riporto il contatore a 1
043500050415     c                   Eval      conta = 1
043600050415      * mi riposiziono sul file delle note all'ultima nota letta e torno a leggere
043700050415     c     kFndcs01      Setll     Fndcs01l
043800050415     c                   Iter
043900050415     c                   EndIf
044000050415     c                   Endsl
044100050415      * campi diversi se primo giro o giri sucessivi
044200050415     c                   If        wgiro = 1
044300050418     c                   Eval      destag = %trim(destag) + ' ' + dcsnot
044400050415     c                   Else
044500050418     c                   Eval      desta1 = %trim(desta1) + ' ' + dcsnot
044600050415     c                   EndIf
044700050415      * incremento il contatore delle note lette
044800050415     c                   Add       1             conta
044900050415     c                   EndDo
045000050415     c                   Select
045100050415      * --> STAMPO ultimo record note
045200050415     c                   When      wnote = *On
045300050517     c                   If        wgiro = 1 and conta > 1
045400050415     c                             and wnote = *On
045500050517     c                   Eval      destag = %trim(destag)
045600050517     c                   Add       1             curline
045700050517     c                   ExSr      Sr_Over
045800050518     c                   If        wover = *On
045900050518     c                   Write     dn91d19
046000050518     c                   EndIf
046100050415     c                   Write     dn91d2
046200050415     c                   EndIf
046300050517     c                   If        wgiro > 1 and conta > 1
046400050415     c                             and wnote = *On
046500050517     c                   Eval      desta1 = %trim(desta1)
046600050517     c                   Add       1             curline
046700050517     c                   ExSr      Sr_Over
046800050518     c                   If        wover = *On
046900050518     c                   Write     dn91d19
047000050518     c                   EndIf
047100050418     c                   Write     dn91d3
047200050415     c                   EndIf
047300050415      * --> STAMPO 2° record (la riga dell'anomalia) se non ho trovato descrizioni aggiuntive
047400050415     c                   other
047500050517     c                   Add       1             curline
047600050415     c                   Write     dn91d2
047700050415     c                   EndSl
047800050428
047900050429      *
    Motivo del danno                   ?
048000050428     c                   Eval      wnote = *Off
048100050428      * cerco le descrizioni aggiuntive
048200050428     c                   Eval      kdcstpd = 'C'
048300050428     c                   Movel(p)  dsnumca       kdcsnkt
048400050428     c                   Eval      kdcsstd = 'M'
048500050428     c                   Clear                   kdcsdim
048600050428     c                   Clear                   kdcshim
048700050428     c                   Clear                   kdcsnks
048800050428     c                   Eval      kdcstrc = 'D'
048900050428     c                   Eval      conta = 1
049000050428     c                   Eval      wgiro = 1
049100050428     c     kFndcs        Setll     Fndcs01l
049200050428     c                   Do        *Hival
049300050428     c     kFndcs        Reade     Fndcs01l
049400050428     c                   If        %Eof(Fndcs01l)
049500050428     c                   Leave
049600050428     c                   EndIf
049700050428     c                   Eval      wnote = *On
049800050428     c                   Select
049900050428      * se primo giro devo stampare 4 note assieme
050000050428     c                   When      wgiro = 1
050100050428      * se ho già caricato 4 note salvo la chiave della nota che sto leggendo x poi riposizionarmi
050200050428     c                   If        conta = 5
050300050428     c                   Eval      kdcstpd = dcstpd
050400050428     c                   Eval      kdcsnkt = dcsnkt
050500050428     c                   Eval      kdcsstd = dcsstd
050600050428     c                   Eval      kdcsdim = dcsdim
050700050428     c                   Eval      kdcshim = dcshim
050800050428     c                   Eval      kdcsnks = dcsnks
050900050428     c                   Eval      kdcstrc = dcstrc
051000050428     c                   Eval      kdcspno = dcspno
051100050428      * --> STAMPO 4° record
051200050517     c                   Eval      desmtv = %trim(desmtv)
051300050517     c                   Add       1             curline
051400050517     c                   ExSr      Sr_Over
051500050518     c                   If        wover = *On
051600050518     c                   Write     dn91d19
051700050518     c                   EndIf
051800050428     c                   Write     dn91d4
051900050428      * riporto il contatore a 1
052000050428     c                   Eval      conta = 1
052100050428     c                   Eval      wgiro = 2
052200050428      * mi riposiziono sul file delle note all'ultima nota letta e torno a leggere
052300050428     c     kFndcs01      Setll     Fndcs01l
052400050428     c                   Iter
052500050428     c                   EndIf
052600050518      * se non è il primo giro devo stampare 5 note assieme
052700050428     c                   Other
052800050518      * se ho già caricato 5 note salvo la chiave della nota che sto leggendo x poi riposizionarmi
052900050518     c                   If        conta = 6
053000050428     c                   Eval      kdcstpd = dcstpd
053100050428     c                   Eval      kdcsnkt = dcsnkt
053200050428     c                   Eval      kdcsstd = dcsstd
053300050428     c                   Eval      kdcsdim = dcsdim
053400050428     c                   Eval      kdcshim = dcshim
053500050428     c                   Eval      kdcsnks = dcsnks
053600050428     c                   Eval      kdcstrc = dcstrc
053700050428     c                   Eval      kdcspno = dcspno
053800050428      * --> STAMPO 5° record
053900050517     c                   Eval      desmtv1 = %trim(desmtv1)
054000050517     c                   Add       1             curline
054100050517     c                   ExSr      Sr_Over
054200050518     c                   If        wover = *On
054300050518     c                   Write     dn91d19
054400050518     c                   EndIf
054500050428     c                   Write     dn91d5
054600050428     c                   Clear                   desmtv1
054700050428      * riporto il contatore a 1
054800050428     c                   Eval      conta = 1
054900050428      * mi riposiziono sul file delle note all'ultima nota letta e torno a leggere
055000050428     c     kFndcs01      Setll     Fndcs01l
055100050428     c                   Iter
055200050428     c                   EndIf
055300050428     c                   Endsl
055400050428      * campi diversi se primo giro o giri sucessivi
055500050428     c                   If        wgiro = 1
055600050428     c                   Eval      desmtv = %trim(desmtv) + ' ' + dcsnot
055700050428     c                   Else
055800050428     c                   Eval      desmtv1 = %trim(desmtv1) + ' ' + dcsnot
055900050428     c                   EndIf
056000050428      * incremento il contatore delle note lette
056100050428     c                   Add       1             conta
056200050428     c                   EndDo
056300050428     c                   Select
056400050428      * --> STAMPO ultimo record note
056500050428     c                   When      wnote = *On
056600050517     c                   If        wgiro = 1 and conta > 1
056700050428     c                             and wnote = *On
056800050517     c                   Eval      desmtv = %trim(desmtv)
056900050517     c                   Add       1             curline
057000050517     c                   ExSr      Sr_Over
057100050518     c                   If        wover = *On
057200050518     c                   Write     dn91d19
057300050518     c                   EndIf
057400050428     c                   Write     dn91d4
057500050428     c                   EndIf
057600050517     c                   If        wgiro > 1 and conta > 1
057700050428     c                             and wnote = *On
057800050517     c                   Eval      desmtv1 = %trim(desmtv1)
057900050517     c                   Add       1             curline
058000050517     c                   ExSr      Sr_Over
058100050518     c                   If        wover = *On
058200050518     c                   Write     dn91d19
058300050518     c                   EndIf
058400050428     c                   Write     dn91d5
058500050428     c                   EndIf
058600050428     c                   EndSl
058700050415
058800050429      *
    Pezzi presenti                     ?
058900050429     c                   Eval      wnote = *Off
059000050429      * cerco le descrizioni aggiuntive
059100050429     c                   Eval      kdcstpd = 'C'
059200050429     c                   Movel(p)  dsnumca       kdcsnkt
059300050429     c                   Eval      kdcsstd = 'K'
059400050429     c                   Clear                   kdcsdim
059500050429     c                   Clear                   kdcshim
059600050429     c                   Clear                   kdcsnks
059700050429     c                   Eval      kdcstrc = 'D'
059800050429     c                   Eval      conta = 1
059900050429     c                   Eval      wgiro = 1
060000050429     c     kFndcs        Setll     Fndcs01l
060100050429     c                   Do        *Hival
060200050429     c     kFndcs        Reade     Fndcs01l
060300050429     c                   If        %Eof(Fndcs01l)
060400050429     c                   Leave
060500050429     c                   EndIf
060600050429     c                   Eval      wnote = *On
060700050429     c                   Select
060800050429      * se primo giro devo stampare 4 note assieme
060900050429     c                   When      wgiro = 1
061000050429      * se ho già caricato 4 note salvo la chiave della nota che sto leggendo x poi riposizionarmi
061100050429     c                   If        conta = 5
061200050429     c                   Eval      kdcstpd = dcstpd
061300050429     c                   Eval      kdcsnkt = dcsnkt
061400050429     c                   Eval      kdcsstd = dcsstd
061500050429     c                   Eval      kdcsdim = dcsdim
061600050429     c                   Eval      kdcshim = dcshim
061700050429     c                   Eval      kdcsnks = dcsnks
061800050429     c                   Eval      kdcstrc = dcstrc
061900050429     c                   Eval      kdcspno = dcspno
062000050429      * --> STAMPO 6° record
062100050517     c                   Eval      desprs = %trim(desprs)
062200050517     c                   Add       1             curline
062300050517     c                   ExSr      Sr_Over
062400050518     c                   If        wover = *On
062500050518     c                   Write     dn91d19
062600050518     c                   EndIf
062700050429     c                   Write     dn91d6
062800050429      * riporto il contatore a 1
062900050429     c                   Eval      conta = 1
063000050429     c                   Eval      wgiro = 2
063100050429      * mi riposiziono sul file delle note all'ultima nota letta e torno a leggere
063200050429     c     kFndcs01      Setll     Fndcs01l
063300050429     c                   Iter
063400050429     c                   EndIf
063500050518      * se non è il primo giro devo stampare 5 note assieme
063600050429     c                   Other
063700050518      * se ho già caricato 5 note salvo la chiave della nota che sto leggendo x poi riposizionarmi
063800050518     c                   If        conta = 6
063900050429     c                   Eval      kdcstpd = dcstpd
064000050429     c                   Eval      kdcsnkt = dcsnkt
064100050429     c                   Eval      kdcsstd = dcsstd
064200050429     c                   Eval      kdcsdim = dcsdim
064300050429     c                   Eval      kdcshim = dcshim
064400050429     c                   Eval      kdcsnks = dcsnks
064500050429     c                   Eval      kdcstrc = dcstrc
064600050429     c                   Eval      kdcspno = dcspno
064700050429      * --> STAMPO 7° record
064800050517     c                   Eval      desprs1 = %trim(desprs1)
064900050517     c                   Add       1             curline
065000050517     c                   ExSr      Sr_Over
065100050518     c                   If        wover = *On
065200050518     c                   Write     dn91d19
065300050518     c                   EndIf
065400050429     c                   Write     dn91d7
065500050429     c                   Clear                   desprs1
065600050429      * riporto il contatore a 1
065700050429     c                   Eval      conta = 1
065800050429      * mi riposiziono sul file delle note all'ultima nota letta e torno a leggere
065900050429     c     kFndcs01      Setll     Fndcs01l
066000050429     c                   Iter
066100050429     c                   EndIf
066200050429     c                   Endsl
066300050429      * campi diversi se primo giro o giri sucessivi
066400050429     c                   If        wgiro = 1
066500050429     c                   Eval      desprs = %trim(desprs) + ' ' + dcsnot
066600050429     c                   Else
066700050429     c                   Eval      desprs1 = %trim(desprs1) + ' ' + dcsnot
066800050429     c                   EndIf
066900050429      * incremento il contatore delle note lette
067000050429     c                   Add       1             conta
067100050429     c                   EndDo
067200050429     c                   Select
067300050429      * --> STAMPO ultimo record note
067400050429     c                   When      wnote = *On
067500050517     c                   If        wgiro = 1 and conta > 1
067600050429     c                             and wnote = *On
067700050517     c                   Eval      desprs = %trim(desprs)
067800050517     c                   Add       1             curline
067900050517     c                   ExSr      Sr_Over
068000050518     c                   If        wover = *On
068100050518     c                   Write     dn91d19
068200050518     c                   EndIf
068300050429     c                   Write     dn91d6
068400050429     c                   EndIf
068500050517     c                   If        wgiro > 1 and conta > 1
068600050429     c                             and wnote = *On
068700050517     c                   Eval      desprs1 = %trim(desprs1)
068800050517     c                   Add       1             curline
068900050517     c                   ExSr      Sr_Over
069000050518     c                   If        wover = *On
069100050518     c                   Write     dn91d19
069200050518     c                   EndIf
069300050429     c                   Write     dn91d7
069400050429     c                   EndIf
069500050429     c                   EndSl
069600050429
069700050429      *
    Pezzi mancanti                     ?
069800050429     c                   Eval      wnote = *Off
069900050429      * cerco le descrizioni aggiuntive
070000050429     c                   Eval      kdcstpd = 'C'
070100050429     c                   Movel(p)  dsnumca       kdcsnkt
070200050429     c                   Eval      kdcsstd = 'P'
070300050429     c                   Clear                   kdcsdim
070400050429     c                   Clear                   kdcshim
070500050429     c                   Clear                   kdcsnks
070600050429     c                   Eval      kdcstrc = 'D'
070700050429     c                   Eval      conta = 1
070800050429     c                   Eval      wgiro = 1
070900050429     c     kFndcs        Setll     Fndcs01l
071000050429     c                   Do        *Hival
071100050429     c     kFndcs        Reade     Fndcs01l
071200050429     c                   If        %Eof(Fndcs01l)
071300050429     c                   Leave
071400050429     c                   EndIf
071500050429     c                   Eval      wnote = *On
071600050429     c                   Select
071700050429      * se primo giro devo stampare 4 note assieme
071800050429     c                   When      wgiro = 1
071900050429      * se ho già caricato 4 note salvo la chiave della nota che sto leggendo x poi riposizionarmi
072000050429     c                   If        conta = 5
072100050429     c                   Eval      kdcstpd = dcstpd
072200050429     c                   Eval      kdcsnkt = dcsnkt
072300050429     c                   Eval      kdcsstd = dcsstd
072400050429     c                   Eval      kdcsdim = dcsdim
072500050429     c                   Eval      kdcshim = dcshim
072600050429     c                   Eval      kdcsnks = dcsnks
072700050429     c                   Eval      kdcstrc = dcstrc
072800050429     c                   Eval      kdcspno = dcspno
072900050429      * --> STAMPO 8° record
073000050517     c                   Eval      desnpz = %trim(desnpz)
073100050517     c                   Add       1             curline
073200050517     c                   ExSr      Sr_Over
073300050518     c                   If        wover = *On
073400050518     c                   Write     dn91d19
073500050518     c                   EndIf
073600050429     c                   Write     dn91d8
073700050429      * riporto il contatore a 1
073800050429     c                   Eval      conta = 1
073900050429     c                   Eval      wgiro = 2
074000050429      * mi riposiziono sul file delle note all'ultima nota letta e torno a leggere
074100050429     c     kFndcs01      Setll     Fndcs01l
074200050429     c                   Iter
074300050429     c                   EndIf
074400050518      * se non è il primo giro devo stampare 5 note assieme
074500050429     c                   Other
074600050518      * se ho già caricato 5 note salvo la chiave della nota che sto leggendo x poi riposizionarmi
074700050518     c                   If        conta = 6
074800050429     c                   Eval      kdcstpd = dcstpd
074900050429     c                   Eval      kdcsnkt = dcsnkt
075000050429     c                   Eval      kdcsstd = dcsstd
075100050429     c                   Eval      kdcsdim = dcsdim
075200050429     c                   Eval      kdcshim = dcshim
075300050429     c                   Eval      kdcsnks = dcsnks
075400050429     c                   Eval      kdcstrc = dcstrc
075500050429     c                   Eval      kdcspno = dcspno
075600050429      * --> STAMPO 9° record
075700050517     c                   Eval      desnpz1 = %trim(desnpz1)
075800050517     c                   Add       1             curline
075900050517     c                   ExSr      Sr_Over
076000050518     c                   If        wover = *On
076100050518     c                   Write     dn91d19
076200050518     c                   EndIf
076300050429     c                   Write     dn91d9
076400050429     c                   Clear                   desnpz1
076500050429      * riporto il contatore a 1
076600050429     c                   Eval      conta = 1
076700050429      * mi riposiziono sul file delle note all'ultima nota letta e torno a leggere
076800050429     c     kFndcs01      Setll     Fndcs01l
076900050429     c                   Iter
077000050429     c                   EndIf
077100050517     c                   EndSl
077200050429      * campi diversi se primo giro o giri sucessivi
077300050429     c                   If        wgiro = 1
077400050429     c                   Eval      desnpz = %trim(desnpz) + ' ' + dcsnot
077500050429     c                   Else
077600050429     c                   Eval      desnpz1 = %trim(desnpz1) + ' ' + dcsnot
077700050429     c                   EndIf
077800050429      * incremento il contatore delle note lette
077900050429     c                   Add       1             conta
078000050429     c                   EndDo
078100050429     c                   Select
078200050429      * --> STAMPO ultimo record note
078300050429     c                   When      wnote = *On
078400050517     c                   If        wgiro = 1 and conta > 1
078500050429     c                             and wnote = *On
078600050517     c                   Eval      desnpz = %trim(desnpz)
078700050517     c                   Add       1             curline
078800050517     c                   ExSr      Sr_Over
078900050518     c                   If        wover = *On
079000050518     c                   Write     dn91d19
079100050518     c                   EndIf
079200050429     c                   Write     dn91d8
079300050429     c                   EndIf
079400050517     c                   If        wgiro > 1 and conta > 1
079500050429     c                             and wnote = *On
079600050517     c                   Eval      desnpz1 = %trim(desnpz1)
079700050517     c                   Add       1             curline
079800050517     c                   ExSr      Sr_Over
079900050518     c                   If        wover = *On
080000050518     c                   Write     dn91d19
080100050518     c                   EndIf
080200050429     c                   Write     dn91d9
080300050429     c                   EndIf
080400050429     c                   EndSl
080500050429
080600050429      *
    N.pezzi mancanti/colli/Kg          ?
080700050429     c                   Eval      pdctnpz = dctnpz
080800050429     c                   Eval      pdctncn = dctncn
080900050429     c                   Eval      pdctpkd = dctpkd
081000050429
081100050429      *
    Verifica DDT                       ?
081200050415     c                   If        dcttdt = 'P'
081300050415     c                   Eval      destdt = 'PARTENZA'
081400050415     c                   EndIf
081500050415     c                   If        dcttdt = 'A'
081600050415     c                   Eval      destdt = 'ARRIVO'
081700050415     c                   EndIf
081800050415     c                   If        dcttdt = *Blanks
081900050415     c                   Eval      destdt = 'NESSUNO'
082000050415     c                   EndIf
082100050429
082200050429      *
    Riserva                            ?
082300050415     c                   If        dctcrc  = 'S'
082400050415     c                   Eval      descrc  = 'SI'
082500050415     c                   EndIf
082600050415     c                   If        dctcrc  <> 'S'
082700050415     c                   Eval      descrc  = 'NO'
082800050415     c                   EndIf
082900050429
083000050429      *
    Disposizioni sulle merce danneg.   ?
083100050415     c                   If        dctdsd  = 'C'
083200050415     c                   Eval      desdsd = 'CONSEGNATA'
083300050415     c                   EndIf
083400050415     c                   If        dctdsd  = 'D'
083500050415     c                   Eval      desdsd = 'DISTRUTTA'
083600050415     c                   EndIf
083700050415     c                   If        dctdsd  = 'R'
083800050415     c                   Eval      desdsd = 'RESA'
083900050415     c                   EndIf
084000050415     c                   If        dctdsd  = 'M'
084100050415     c                   Eval      desdsd = 'MAGAZZINO'
084200050415     c                   EndIf
084300050412
084400050429      *
    Beneficiario alternativo           ?
084500050412     c                   If        dctksc > *Zeros
084600050415     c                   Eval      pdctksc = dctksc
084700050412     c                   Move      dctksc        w0040
084800050412      *   se codificato decodifico da p.d.c.
084900050412     c                   If        w0040 <> 8888
085000050412     c                   Clear                   tibs69ds
085100050412     c                   z-add     dctksc        I69kac
085200050412     c                   Call      'TIBS69R'
085300050412     c                   Parm                    tibs69DS
085400050412     c                   Parm                    ds_cnaco
085500050412     c                   Parm                    ds_cnind
085600050412     c                   Parm                    ds_cnclp
085700050412     c                   Parm                    ds_fncls
085800050429     c                   If        o69err <> '1'
085900050412     c                   Eval      desksc = acorag
086000050412     c                   EndIf
086100050412     c                   Else
086200050412      *   se non codificato decodifico da anagrafica clienti c.a.
086300050412     c                   Eval      wtrc = 'C'
086400050412     c     kFndka        Chain     Fndka01l
086500050414     c                   If        %Found(Fndka01l) and dkaatb <> 'A'
086600050412     c                   Eval      desksc = dkarag
086700050412     c                   EndIf
086800050412     c                   EndIf
086900050412     c                   EndIf
087000050415
087100050429      *
    Interlocutore                      ?
087200050415     c                   Eval      kdcstpd = 'C'
087300050415     c                   Movel(p)  dsnumca       kdcsnkt
087400050415     c                   Eval      kdcsstd = 'N'
087500050415     c                   Clear                   kdcsdim
087600050415     c                   Clear                   kdcshim
087700050415     c                   Clear                   kdcsnks
087800050415     c                   Eval      kdcstrc = 'D'
087900050415     c     kFndcs        Chain     Fndcs01l
088000050415     c                   If        %Found(Fndcs01l)
088100050415     c                   Eval      desint = dcsnot
088200050415     c                   EndIf
088300050415
088400050429      *
    Responsabilità internazionale      ?
088500050415     c                   Eval      *In02 = *Off
088600050415     c                   If        dctres = 'V'
088700050415     c                   Eval      desres = 'VETTORE'
088800050415     c                   Eval      *In02 = *On
088900050415     c                   EndIf
089000050415     c                   If        dctres = 'P'
089100050415     c                   Eval      desres = 'PARTNER'
089200050415     c                   Eval      *In02 = *On
089300050415     c                   EndIf
089400050414
089500050429      *
    Rivalsa                            ?
089600050414     c                   Eval      *In01 = *Off
089700050414     c                   Eval      wtrc = 'R'
089800050414     c     kFndka        Chain     Fndka01l
089900050414     c                   If        %Found(Fndka01l) and dkaatb <> 'A'
090000050414     c                   Eval      *In01 = *On
090100050414     c                   Eval      desass = dkarag
090200050414     c                   Eval      codass = dkaksc
090300050414     c                   Eval      viaass = dkavia
090400050414     c                   Eval      capass = dkacap
090500050414     c                   Eval      locass = dkaloc
090600050414     c                   Eval      prvass = dkaprv
090700050414     c                   Eval      nazass = dkanaz
090800050414     c                   Eval      telass = dkatel
090900050414     c                   Eval      faxass = dkafax
091000050414     c                   EndIf
091100050429
091200050429      *
    Causale chiusura C.A.              ?
091300050429     c                   Eval      pdctcch = dctcch
091400050429     c                   Clear                   dcch
091500050429     c                   Clear                   tibs02ds
091600050429     c                   Eval      t02mod = 'C'
091700050429     c                   Eval      t02sif = knsif
091800050429     c                   Eval      t02cod = 'CCH'
091900050429     c                   Eval      t02ke1 = dctcch
092000050429     c                   Call      'TIBS02R'
092100050429     c                   Parm                    kpjba
092200050429     c                   Parm                    tibs02ds
092300050429     c                   If        t02err = *Blanks
092400050429     c                   Eval      dcch = t02uni
092500050429     c                   EndIf
092600050429     c                   Eval      descch = §cchdesc
092700050429
092800050429      *
    Pagamento                          ?
092900050429      * controllo se è stata fatta almeno un'estrazione o il pagamento
093000050429     c                   If        §dctpaga = 'C'
093100050429     c                   Eval      despag = 'Estratta per il pagamento'
093200050429     c                   EndIf
093300050429     c                   If        §dctpaga = 'P'
093400050429     c                   Eval      despag = 'Pagata'
093500050429     c                   EndIf
093600050429
093700050429      *
    Mandato assicurativo               ?
093800050503     c                   Eval      *In05 = *Off
093900050429     c     kFndc         Chain     Fndpt01l
094000050429     c                   If        %Found(Fndpt01l) and dptatb <> 'A'
094100050429     c                   Eval      *In05 = *On
094200050429     c                   Eval      pdptksc = dptksc
094300050429     c                   If        dptftc = *Blanks
094400050429     c                   Eval      desman = 'CLIENTE SENZA MANDATO'
094500050429     c                   Else
094600050429     c                   Clear                   ds1p
094700050429     c                   Eval      ktblcod = '1P'
094800050429     c                   Eval      ktblkey = dptftc
094900050429     c     kTab          Chain     Tabel00f
095000050429     c                   If        %Found(tabel00f)
095100050429     c                   Eval      ds1p = tbluni
095200050429     c                   EndIf
095300050429     c                   Eval      desman = §1pdes
095400050429     c                   EndIf
095500050429      * Ragione sociale cliente
095600050429     c                   Clear                   tibs69ds
095700050429     c                   z-add     dptksc        I69kac
095800050429     c                   Call      'TIBS69R'
095900050429     c                   Parm                    tibs69DS
096000050429     c                   Parm                    ds_cnaco
096100050429     c                   Parm                    ds_cnind
096200050429     c                   Parm                    ds_cnclp
096300050429     c                   Parm                    ds_fncls
096400050429     c                   If        o69err <> '1'
096500050429     c                   Eval      ragdpt = acorag
096600050429     c                   EndIf
096700050429      * codice tariffa
096800050429     c                   Eval      pdptctr = dptctr
096900050429     c                   Clear                   dstr
097000050429     c                   Eval      ktblcod = 'TR'
097100050429     c                   Movel     dptctr        w001a
097200050429     c                   Eval      ktblkey = w001a
097300050429     c     kTab          Chain     Tabel00f
097400050429     c                   If        %Found(tabel00f)
097500050429     c                   Eval      dstr = tbluni
097600050429     c                   EndIf
097700050429     c                   Eval      desctr = §trdes
097800050429      * tipo valore merce
097900050429     c                   Eval      pdptvlm = dptvlm
098000050429     c                   Eval      pdptvvm = dptvvm
098100050429     c                   Clear                   dstr
098200050429     c                   Eval      ktblcod = 'TR'
098300050429     c                   Eval      ktblkey = dptfvm
098400050429     c     kTab          Chain     Tabel00f
098500050429     c                   If        %Found(tabel00f)
098600050429     c                   Eval      dstr = tbluni
098700050429     c                   EndIf
098800050429     c                   Eval      desvvm = §trde3
098900050429     c                   EndIf
099000050429
099100050429      * --> STAMPO 10° record
099200050517     c                   Add       3             curline
099300050517     c   01              Add       1             curline
099400050517     c   05              Add       1             curline
099500050517     c                   ExSr      Sr_Over
099600050518     c                   If        wover = *On
099700050518     c                   Write     dn91d19
099800050518     c                   EndIf
099900050429     c                   Write     dn91d10
100000050415
100100050429      *
    Importo da assicurare              ?
100200050415     c                   Eval      ptasias = tasias
100300050415     c                   Eval      ptasvas = tasvas
100400050415      * se inserito da fatturazione non stampo l'importo da assicurare
100500050415     c                   If        tasfcm = 'F'
100600050415     c                   Eval      *In03 = *Off
100700050415     c                   Else
100800050415     c                   Eval      *In03 = *On
100900050415     c                   EndIf
101000050415
101100050429      *
    Importo Max risarcibile            ?
101200050415     c                   Eval      pdctipv = dctipv
101300050415     c                   Eval      pdctvpv = dctvpv
101400050415      * Tipo inserimento importo max risarcibile
101500050413     c                   If        dctfpv = 'M'
101600050413     c                   Eval      desfpv = 'INSERITO MANUALMENTE'
101700050413     c                   EndIf
101800050413     c                   If        dctfpv = 'C'
101900050413     c                   Eval      desfpv = 'CALCOLATO'
102000050413     c                   EndIf
102100050415
102200050415      * Aggancio file Liquidazione C.A.
102300050415     c     kFndc         Chain     Fndcl01l
102400050415     c                   If        not %Found(Fndcl01l)
102500050418     c                   ExSr      Puldcl
102600050415     c                   EndIf
102700050415
102800050429      *
    Importo richiesto                  ?
102900050415     c                   Eval      pdclipr = dclipr
103000050415     c                   Eval      pdclvpr = dclvpr
103100050413
103200050429      *
    Importo trattenuto                 ?
103300050415     c                   Eval      pdclipt = dclipt
103400050415     c                   Eval      pdclvpt = dclvpt
103500050415      * data importo trattenuto
103600050414     c                   If        dcldpt > *Zeros
103700050414     c     *iso          Move      dcldpt        wdataeur
103800050414     c     *eur          Movel     wdataeur      datapt
103900050414     c                   EndIf
104000050415
104100050429      *
    Importo liquidato                  ?
104200050418      * se è pratica è in un campo... se è liquidazione in un altro
104300050418     c                   If        *In20
104400050418     c                   Eval      pdclipl = dclipl
104500050418     c                   Eval      pdclvpl = dclvpl
104600050418     c                   Else
104700050418     c                   If        *In21
104800050418     c                   Eval      impipp = dclipl
104900050418     c                   Eval      valipp = dclvpl
105000050418     c                   EndIf
105100050418      * se pratica trattenuta da cliente e pagamento già avvenuto imposto come importo
105200050418      * liquidato la differenza
105300050418     c                   If        dclipl <= dclipt
105400050418     c                   Clear                   pdclipl
105500050418     c                   Clear                   pdclvpl
105600050418     c                   Else
105700050418     c                   Eval      pdclipl = dclipl -dclipt
105800050418     c                   Eval      pdclvpl = dclvpl
105900050418     c                   EndIf
106000050418     c                   EndIf
106100050415     c
106200050414
106300050429      *
    Data emissione assegno             ?
106400050414     c                   If        dcldea > *Zeros
106500050414     c     *iso          Move      dcldea        wdataeur
106600050414     c     *eur          Movel     wdataeur      dataea
106700050414     c                   EndIf
106800050414
106900050429      *
    Decodifica ABI/CAB                 ?
107000050414     c                   Move      dclabi        wabi
107100050414     c                   Move      dclcab        wcab
107200050414     c     kCnabi        Chain     Cnabi01l
107300050414     c                   If        %Found (Cnabi01l)
107400050414     c                   Eval      desban = %trim(abiist) + '-' + %trim(abiage)
107500050414     c                             + '-' + %trim(abiloc) + '-' + %trim(abicom)
107600050414     c                   EndIf
107700050418
107800050429      * --> STAMPO 11° record
107900050517     c                   Add       2             curline
108000050517     c                   ExSr      Sr_Over
108100050518     c                   If        wover = *On
108200050518     c                   Write     dn91d19
108300050518     c                   EndIf
108400050429     c                   Write     dn91d11
108500050414
108600050429      *
    Intestatario progetto di liquidaz. ?
108700050418      * Solo se è Pratica Assicurativa o Liquidazione Transattiva
108800050418     c                   If        *In20 or *In21
108900050414     c                   Eval      wtrc = 'I'
109000050414     c     kFndka        Chain     Fndka01l
109100050414     c                   If        %Found(Fndka01l) and dkaatb <> 'A'
109200050414     c                   Eval      despli = dkarag
109300050414     c                   Eval      codpli = dkaksc
109400050414     c                   Eval      viapli = dkavia
109500050429     c                   Eval      locpli = %trim(dkacap) + ' ' + %trim(dkaloc)
109600050429     c                                    + '  ' + dkaprv + ' ' + dkanaz
109700050414     c                   EndIf
109800050414
109900050429      *
    Data stp. progetto di liquidazione ?
110000050414     c                   If        dcldds > *Zeros
110100050414     c     *iso          Move      dcldds        wdataeur
110200050414     c     *eur          Movel     wdataeur      datads
110300050414     c                   EndIf
110400050429
110500050429      *
    Data accett. prg. di liquidazione  ?
110600050414      * controllo la validità della data xchè ce ne sono delle farlocche soprattutto
110700050414      * nella vecchie c.a.
110800050504      * se errata metto data valuta pagamento
110900050414     c                   If        dcldpl > *Zeros
111000050414     c                   clear                   wlbdat
111100050419     c                   Z-add     dcldpl        g02inv
111200050419     c                   Eval      g02err = '3'
111300050414     c                   call      'XSRDA8'
111400050414     c                   parm                    wlbdat
111500050419     c                   If        g02err = '1'
111600050414     c     *iso          Move      dclvlp        wdataeur
111700050414     c     *eur          Movel     wdataeur      datapl
111800050414     c                   Else
111900050510     c                   Move      g02dat        datapl
112000050414     c                   EndIf
112100050414     c                   EndIf
112200050414
112300050429      *
    Data valuta pagamento              ?
112400050504      * controllo la validità della data xchè ce ne sono delle farlocche soprattutto
112500050504      * nella vecchie c.a.
112600050504      * se errata metto data accett. prg. di liquidazione
112700050414     c                   If        dclvlp > *Zeros
112800050504     c                   clear                   wlbdat
112900050510     c                   Z-add     dclvlp        g02inv
113000050504     c                   Eval      g02err = '3'
113100050504     c                   call      'XSRDA8'
113200050504     c                   parm                    wlbdat
113300050504     c                   If        g02err = '1'
113400050504     c     *iso          Move      dcldpl        wdataeur
113500050504     c     *eur          Movel     wdataeur      datalp
113600050504     c                   Else
113700050510     c                   Move      g02dat        datalp
113800050414     c                   EndIf
113900050504     c                   EndIf
114000050418     c                   EndIf
114100050429
114200050429      *
    Note calcolo progetto di liq.      ?
114300050429      *     solo 4 note
114400050503     c                   Eval      *In06 = *Off
114500050429     c                   If        dcldds > *Zeros
114600050429     c                   Eval      kdcstpd = 'C'
114700050429     c                   Movel(p)  dsnumca       kdcsnkt
114800050429     c                   Eval      kdcsstd = 'C'
114900050429     c                   Clear                   kdcsdim
115000050429     c                   Clear                   kdcshim
115100050429     c                   Clear                   kdcsnks
115200050429     c                   Eval      kdcstrc = 'D'
115300050429     c     kFndcs        Setll     Fndcs01l
115400050429     c                   Do        *Hival
115500050429     c     kFndcs        Reade     Fndcs01l
115600050429     c                   If        %Eof(Fndcs01l)
115700050429     c                   Leave
115800050429     c                   EndIf
115900050429     c                   Eval      desplt = %trim(desplt) + ' ' + dcsnot
116000050429     c                   Eval      *In06 = *On
116100050429     c                   EndDo
116200050429     c                   EndIf
116300050418
116400050429      * --> STAMPO 12° record
116500050517     c                   Add       2             curline
116600050517     c   06              Add       1             curline
116700050517     c                   ExSr      Sr_Over
116800050518     c                   If        wover = *On
116900050518     c                   Write     dn91d19
117000050518     c                   EndIf
117100050429     c                   Write     dn91d12
117200070104
117300070104      *
    RECUPERO DA A.I.G.                 ?
117400090414     c*****              clear                   datarr
117500090414     c*****              clear                   datavr
117600090414     c*****              clear                   datard
117700070104      * controllo se esite la C.A. sul file dei recuperi A.I.G.
117800090414     c*****kfndc         chain     fndra01l
117900090414     c*****              if        %found (fndra01l) and draatb = ' '
118000070104      * verifico se CA esclusa dal rimborso
118100090414     c*****              if          draesc = 'E'
118200070104      * --> STAMPO 22° record
118300090414     c*****              add       1             curline
118400090414     c*****              exsr      sr_over
118500090414     c*****              if        wover = *on
118600090414     c*****              write     dn91d19
118700090414     c*****              endif
118800090414     c*****              write     dn91d22
118900070104
119000070104      * se non è esclusa stampo i dati del recupero
119100090414     c*****              else
119200070104      * data richiesta recupero
119300090414     c*****              if        dradrr > *zeros
119400090414     c******iso          move      dradrr        wdataeur
119500090414     c******eur          movel     wdataeur      datarr
119600090414     c*****              endif
119700070104      * data ricezione recupero
119800090414     c*****              if        dradvr > *zeros
119900090414     c******iso          move      dradvr        wdataeur
120000090414     c******eur          movel     wdataeur      datavr
120100090414     c*****              endif
120200070104      * --> STAMPO 20° record
120300090414     c*****              add       1             curline
120400090414     c*****              exsr      sr_over
120500090414     c*****              if        wover = *on
120600090414     c*****              write     dn91d19
120700090414     c*****              endif
120800090414     c*****              write     dn91d20
120900070104
121000070104      * cerco anche le disposizioni A.I.G.
121100090414     c*****              eval      kdcstpd = 'C'
121200090414     c*****              movel(p)  dsnumca       kdcsnkt
121300090414     c*****              eval      kdcsstd = 'D'
121400090414     c*****              clear                   kdcsdim
121500090414     c*****              clear                   kdcshim
121600090414     c*****              clear                   kdcsnks
121700090414     c*****              eval      kdcstrc = 'D'
121800090414     c*****              eval      conta = 1
121900090414     c*****              eval      wgiro = 1
122000090414     c*****kfndcs        setll     fndcs01l
122100090414     c*****              do        *hival
122200090414     c*****kfndcs        reade     fndcs01l
122300090414     c*****              if        %eof(fndcs01l)
122400090414     c*****              leave
122500090414     c*****              endif
122600090414     c*****              if        dcsatb = 'A'
122700090414     c*****              iter
122800090414     c*****              endif
122900090414     c*****              eval      *in08 = *off
123000070104      * se ho già caricato 3 note nel campo nota
123100070104      * stampo
123200090414     c*****              if        conta = 4
123300070104      * se non è la prima volta che stampo non stampo la data
123400090414     c*****              if        wgiro > 1
123500090414     c*****              eval      *in08 = *on
123600090414     c*****              endif
123700070104      * se è la prima volta che stampo devo stampare la data
123800090414     c*****              if        wgiro = 1
123900090414     c*****              add       1             wgiro
124000090414     c*****              endif
124100090414     c*****              eval      rigadisp = %trim(rigadisp)
124200070104      * --> STAMPO 21° record
124300090414     c*****              add       1             curline
124400090414     c*****              exsr      sr_over
124500090414     c*****              if        wover = *on
124600090414     c*****              write     dn91d19
124700090414     c*****              endif
124800090414     c*****              write     dn91d21
124900090414     c*****              clear                   rigadisp
125000090414     c*****              eval      conta = 1
125100090414     c*****              endif
125200070104      * data ricezione disposizioni
125300090414     c*****              if        dradrd > *zeros
125400090414     c******iso          move      dradrd        wdataeur
125500090414     c******eur          movel     wdataeur      datard
125600090414     c*****              endif
125700070104      * imposto la riga della nota 3 note x riga
125800090414     c*****              eval      rigadisp = %trim(rigadisp) + ' ' + dcsnot
125900090414     c*****              add       1             conta
126000090414     c*****              enddo
126100070104      * se a fine lettura ho ancora delle note da stampare le stampo
126200090414     c*****              if        conta > 1
126300090414     c*****              if        wgiro > 1
126400090414     c*****              eval      *in08 = *on
126500090414     c*****              endif
126600090414     c*****              eval      rigadisp = %trim(rigadisp)
126700090414     c*****              add       1             curline
126800090414     c*****              exsr      sr_over
126900090414     c*****              if        wover = *on
127000090414     c*****              write     dn91d19
127100090414     c*****              endif
127200090414     c*****              write     dn91d21
127300090414     c*****              endif
127400070104
127500090414     c*****              endif
127600090414     c*****              endif
127700050414
127800050429      *
    F A S I                            ?
127900050503     c                   Clear                   conta
128000050504     c                   Clear                   s_rigafase
128100050503      * --> STAMPO 13° record
128200050418      *     intestazione fasi
128300050517     c                   Add       3             curline
128400050517     c                   ExSr      Sr_Over
128500050503     c                   Write     dn91d13
128600050418
128700050418      * --> Fasi eseguite
128800050414     c     kFndc         Setll     Fndcf01l
128900050414     c                   Do        *Hival
129000050414     c     kFndc         Reade     Fndcf01l
129100050414     c                   If        %Eof(Fndcf01l)
129200050414     c                   Leave
129300050414     c                   EndIf
129400050414     c                   If        dcfatb = 'A'
129500050414     c                   Iter
129600050414     c                   EndIf
129700050504
129800050504     c                   Add       1             conta
129900050503
130000050503      * Descrizione fase
130100050503     c     dcffca        Chain     Fndfa01l
130200050503     c                   If        Not %Found(Fndfa01l)
130300050503     c                   Clear                   dfades
130400050503     c                   EndIf
130500050503
130600050503     c                   Eval      ddcf01 = dcflet
130700050503
130800050503      * Data esecuzione fase
130900050503     c     *iso          Move      dcfdfc        wdataeur
131000050504     c     *eur          Movel     wdataeur      datafa
131100050504
131200050504      * Imposto i campi come devono essere stampati
131300050504      * data/ora fase/descrizione flag p.o./utente
131400050504     c                   Eval      rigafase1 = %editw(datafa:'  /  /    ') +
131500050504     c                             ' ' + %editw(dcfhfc:'  :  ') + ' ' +
131600050504     c                             %editc(dcffca:'1') + ' ' + dfades + '   ' +
131700050504     c                             §dcfpfra + '    ' + dcfptr + '    ' +
131800050504     c                             %editc(dcffev:'1') + '  ' + dcfpru
131900050504
132000050504     c                   Eval      s_rigafase(conta) = rigafase1
132100050414
132200050414     c                   EndDo
132300050503
132400050504      * Stampo le fasi su due colonne
132500050504     c     conta         div       2             righe
132600050504     c                   mvr                     resto
132700050504     c                   add       resto         righe
132800050504     c                   Clear                   col1
132900050504     c                   Clear                   col2
133000050504     c                   Do        righe         col1
133100050504     c                   Eval      col2 = col1 + righe
133200050504     c                   Clear                   dn91d14
133300050504     c                   Eval      rigafase1 = s_rigafase(col1)
133400050504     c                   Eval      rigafase2 = s_rigafase(col2)
133500050503      * --> STAMPO 14° record
133600050517     c                   Add       1             curline
133700050517     c                   ExSr      Sr_Over
133800050518     c                   If        wover = *On
133900050518     c                   Write     dn91d13
134000050518     c                   EndIf
134100050503     c                   Write     dn91d14
134200050504     c                   EndDo
134300050414
134400050503      *
    Documenti Richiesti                ?
134500050509     c                   Clear                   conta
134600050509     c                   Clear                   s_rigadoc
134700050509     c     kFndc         Setll     Fndcc01l
134800050509     c                   If        %Equal(Fndcc01l)
134900050509      * --> STAMPO 15° record
135000050509      *     intestazione documenti
135100050517     c                   Add       3             curline
135200050517     c                   ExSr      Sr_Over
135300050509     c                   Write     dn91d15
135400050509
135500050509     c                   Do        *Hival
135600050509     c     kFndc         Reade     Fndcc01l
135700050509     c                   If        %Eof(Fndcc01l)
135800050509     c                   Leave
135900050509     c                   EndIf
136000050509     c                   If        dccatb = 'A'
136100050509     c                   Iter
136200050509     c                   EndIf
136300050509
136400050509     c                   Add       1             conta
136500050509
136600050509      * Descrizione tipo documento
136700050509     c                   Clear                   dcdd
136800050509     c                   Clear                   tibs02ds
136900050509     c                   Eval      t02mod = 'C'
137000050509     c                   Eval      t02sif = knsif
137100050509     c                   Eval      t02cod = 'CDD'
137200050509     c                   Eval      t02ke1 = dcccdd
137300050509     c                   Call      'TIBS02R'
137400050509     c                   Parm                    kpjba
137500050509     c                   Parm                    tibs02ds
137600050509     c                   If        t02err = *Blanks
137700050509     c                   Eval      dcdd = t02uni
137800050509     c                   EndIf
137900050509
138000050510     c                   Clear                   datarz
138100050510     c                   Clear                   datadc
138200050509      * Data ricezione documento
138300050510      * controllo la validità della data xchè ce ne sono delle farlocche soprattutto
138400050510      * nella vecchie c.a.
138500050510      * se errata la azzero e non la stampo
138600050509     c                   If        dccdrz > *Zeros
138700050510     c                   clear                   wlbdat
138800050510     c                   Z-add     dccdrz        g02inv
138900050510     c                   Eval      g02err = '3'
139000050510     c                   call      'XSRDA8'
139100050510     c                   parm                    wlbdat
139200050510     c                   If        g02err = '1'
139300050510     c                   Clear                   datarz
139400050510     c                   Else
139500050510     c                   Move      g02dat        datarz
139600050510     c                   EndIf
139700050509     c                   EndIf
139800050509
139900050509      * Imposto i campi come devono essere stampati
140000050509      * data/tipo doc/descrizione
140100050509     c                   Eval      rigadoc1 = %editw(datarz:'  /  /    ') +
140200050509     c                             ' ' + dcccdd + ' ' + §cdddesc
140300050510     c                   Eval      s_rigadoc(conta) = rigadoc1
140400050509
140500050509      * Cerco i dati previsti per il documento
140600050509     c                   If        §cddpnum = 'S'
140700050509      * e se è già pervenuto altrimenti non faccio vedere i campi vuoti
140800050509     c                             and dccdrz > 0
140900050510
141000050510     c                   Add       1             conta
141100050509      * data documento
141200050509     c                   If        dclddc > *Zeros
141300050509     c     *iso          Move      dclddc        wdataeur
141400050509     c     *eur          Movel     wdataeur      datadc
141500050510     c                   EndIf
141600050509      * Imposto i campi come devono essere stampati
141700050509      * numero doc./data/importo/valuta
141800050510     c                   Eval      rigadoc1 = w014a + 'Numero:' + ' ' +
141900050509     c                                      %trim(%editc(dclndc:'4')) + ' ' +
142000050509     c                                     'Data documento:' + ' ' +
142100050509     c                                      %trim(%editw(datadc:'  /  /    ')) +
142200050509     c                                     ' ' + 'Importo:' + ' ' +
142300050509     c                                      %trim(%editc(dclipr:'4')) + ' ' +
142400050509     c                                      dclvpr
142500050510     c                   Eval      s_rigadoc(conta) = rigadoc1
142600050509     c                   EndIf
142700050509
142800050509     c                   EndDo
142900050509
143000050509      * Stampo i documenti su due colonne
143100050509     c     conta         div       2             righe
143200050509     c                   mvr                     resto
143300050509     c                   add       resto         righe
143400050509     c                   Clear                   col1
143500050509     c                   Clear                   col2
143600050509     c                   Do        righe         col1
143700050509     c                   Eval      col2 = col1 + righe
143800050509     c                   Clear                   dn91d16
143900050509     c                   Eval      rigadoc1 = s_rigadoc(col1)
144000050509     c                   Eval      rigadoc2 = s_rigadoc(col2)
144100050509      * --> STAMPO 16° record
144200050517     c                   Add       1             curline
144300050517     c                   ExSr      Sr_Over
144400050518     c                   If        wover = *On
144500050518     c                   Write     dn91d15
144600050518     c                   EndIf
144700050509     c                   Write     dn91d16
144800050509     c                   EndDo
144900050509     c                   EndIf
145000050516
145100050516      *
    Note generiche                     ?
145200050516     c                   Eval      kdcstpd = 'C'
145300050516     c                   Movel(p)  dsnumca       kdcsnkt
145400050516     c                   Clear                   kdcsstd
145500050517     c                   Eval      conta = 1
145600050517     c                   Eval      wgiro = 1
145700050517     c                   Clear                   oldhim
145800050516     c     kFndcs02      Setll     Fndcs01l
145900050516     c                   If        %Equal(Fndcs01l)
146000050516      * --> STAMPO 17° record
146100050516      *     intestazione note
146200050517     c                   Add       3             curline
146300050517     c                   ExSr      Sr_Over
146400050516     c                   Write     dn91d17
146500050516
146600050516     c                   Do        *Hival
146700050516     c     kFndcs02      Reade     Fndcs01l
146800050516     c                   If        %Eof(Fndcs01l)
146900050516     c                   Leave
147000050516     c                   EndIf
147100050516     c                   If        dcsatb = 'A'
147200050516     c                   Iter
147300050516     c                   EndIf
147400050517
147500050517     c                   Eval      *In07 = *Off
147600050516
147700050517      * se stessa data/ora immissione ed ho già caricato 4 note nel campo nota
147800050517      * stampo
147900050517     c                   If        conta = 5 and
148000050517     c                             (oldhim = dcshim and oldhim > *Zeros)
148100050517      * se non è la prima volta che stampo non stampo la data/ora immissione e la fase
148200050517     c                   If        wgiro > 1
148300050517     c                   Eval      *In07 = *On
148400050517     c                   EndIf
148500050517      * se è la prima volta che stampo devo stampare la data/ora immissione e la fase
148600050517     c                   If        wgiro = 1
148700050517     c                   Add       1             wgiro
148800050517     c                   EndIf
148900050517     c                   Eval      riganota = %trim(riganota)
149000050517      * --> STAMPO 18° record
149100050517     c                   Add       1             curline
149200050517     c                   ExSr      Sr_Over
149300050518     c                   If        wover = *On
149400050518     c                   Write     dn91d17
149500050518     c                   EndIf
149600050517     c                   Write     dn91d18
149700050517     c                   Clear                   riganota
149800050517     c                   Eval      conta = 1
149900050517     c                   EndIf
150000050517
150100050517      * a rottura di ora immissione stampo le note
150200050517     c                   If        oldhim <> dcshim
150300050517     c                   If        oldhim > *Zeros
150400050517      * se non è la prima volta che stampo non stampo la data/ora immissione e la fase
150500050517     c                   If        wgiro > 1
150600050517     c                   Eval      *In07 = *On
150700050517     c                   EndIf
150800050517     c                   Eval      riganota = %trim(riganota)
150900050517      * --> STAMPO 18° record
151000050517     c                   Add       1             curline
151100050517     c                   ExSr      Sr_Over
151200050518     c                   If        wover = *On
151300050518     c                   Write     dn91d17
151400050518     c                   EndIf
151500050517     c                   Write     dn91d18
151600050517     c                   Clear                   riganota
151700050517     c                   Eval      conta = 1
151800050517     c                   Eval      wgiro = 1
151900050517     c                   EndIf
152000050517     c                   Eval      oldhim = dcshim
152100050517     c                   EndIf
152200050516
152300050516     c                   Clear                   datanot
152400050516      * Data nota
152500050516     c     *iso          Move      dcsdim        wdataeur
152600050516     c     *eur          Movel     wdataeur      datanot
152700050516
152800050516      * Fase nota
152900050516     c                   Eval      pfaseno = %subst(dcsnks:1:3)
153000050517      * P.o. nota e utente
153100050517     c                   Eval      pdcspos = dcspos
153200050517     c                   Eval      pdcspru = dcspru
153300050516
153400050516      * Imposto la riga della nota 4 note x riga
153500050517     c                   Eval      riganota = %trim(riganota) + ' ' + dcsnot
153600050517     c                   Add       1             conta
153700050516
153800050516     c                   EndDo
153900050517
154000050517      * se a fine lettura ho ancora delle note da stampare le stampo
154100050517     c                   If        conta > 1
154200050517     c                   If        wgiro > 1
154300050517     c                   Eval      *In07 = *On
154400050517     c                   EndIf
154500050517     c                   Eval      riganota = %trim(riganota)
154600050517     c                   Add       1             curline
154700050517     c                   ExSr      Sr_Over
154800050518     c                   If        wover = *On
154900050518     c                   Write     dn91d17
155000050518     c                   EndIf
155100050517     c                   Write     dn91d18
155200050517     c                   EndIf
155300050516
155400050516     c                   EndIf
155500050408
155600050411     c                   EndSr
155700050517
155800050517      *------------------------------------------------------------------------*
155900050517      * GESTIONE SALTO PAGINA
156000050517      *------------------------------------------------------------------------*
156100050517     c     Sr_Over       BegSr
156200050517
156300050518     c                   Eval      wover = *Off
156400050517     c                   If        *In98 or curline >= 63
156500050517     c                   Write     dn91t1
156600050517     c                   Write     dn91d1
156700050517     c                   Eval      *In98 = *Off
156800050518     c                   Eval      wover = *On
156900050517     c                   EndIf
157000050517
157100050517     c                   EndSr
157200050414
157300050414      *------------------------------------------------------------------------*
157400050414      *  PULIZIA CAMPI
157500050414      *------------------------------------------------------------------------*
157600050414     c     Pulisci       BegSr
157700050414
157800050415     c                   Clear                   dn91d1
157900050415     c                   Clear                   dn91d2
158000050415     c                   Clear                   dn91d3
158100050415     c                   Clear                   dn91d4
158200050415     c                   Clear                   dn91d5
158300050415     c                   Clear                   dn91d6
158400050418     c                   Clear                   dn91d7
158500050418     c                   Clear                   dn91d8
158600050418     c                   Clear                   dn91d9
158700050418     c                   Clear                   dn91d10
158800050418     c                   Clear                   dn91d11
158900050418     c                   Clear                   dn91d12
159000090414     c*****              clear                   dn91d20
159100090414     c*****              clear                   dn91d21
159200090414     c*****              clear                   dn91d22
159300050418     c                   Clear                   dn91d13
159400050418     c                   Clear                   dn91d14
159500050504     c                   Clear                   dn91d15
159600050504     c                   Clear                   dn91d16
159700050516     c                   Clear                   dn91d17
159800050516     c                   Clear                   dn91d18
159900050414
160000050414     c                   EndSr
160100050414
160200050418      *------------------------------------------------------------------------*
160300050418      *  PULIZIA CAMPI FILE FNDCL
160400050418      *------------------------------------------------------------------------*
160500050418     c     Puldcl        BegSr
160600050418
160700050418     c                   Clear                   dclipr
160800050418     c                   Clear                   dclvpr
160900050418     c                   Clear                   dclipt
161000050418     c                   Clear                   dclvpt
161100050418     c                   Clear                   dcldpt
161200050418     c                   Clear                   dclipl
161300050418     c                   Clear                   dclvpl
161400050418     c                   Clear                   dclndc
161500050418     c                   Clear                   dclddc
161600050418     c                   Clear                   dclvlp
161700050418     c                   Clear                   dcldpl
161800050418     c                   Clear                   dcldds
161900050418     c                   Clear                   dclabi
162000050418     c                   Clear                   dclcab
162100050418     c                   Clear                   dclasn
162200050418     c                   Clear                   dcldea
162300050418
162400050418     c                   EndSr
162500050418
162600050411      *------------------------------------------------------------------------*
162700050411      *  CONTROLLO TIPO BOLLA
162800050411      *------------------------------------------------------------------------*
162900050411     c     Ctrtbl        BegSr
163000050411
163100050411     c                   Clear                   dstb
163200050412     c                   Eval      ktblcod = 'TB'
163300050412     c                   Eval      ktblkey = tastbl
163400050411     c     kTab          Chain     Tabel00f
163500050411     c                   If        %Found(tabel00f)
163600050411     c                   Eval      dstb = tbluni
163700050411     c                   If        §tbrbl <> 'R'
163800050411     c                   Eval      flgtb1 = §tbtpo
163900050411     c                   EndIf
164000050411     c                   EndIf
164100050411
164200050411     c                   EndSr
164300050414
164400050411      *------------------------------------------------------------------------*
164500050411      * ROUTINE INIZIALE
164600050411      *------------------------------------------------------------------------*
164700050411     c     *Inzsr        BegSr
164800050411
164900050411     c     *Entry        Plist
165000050411     c                   Parm                    kpjba
165100050411     c                   Movel     kpjbu         parm01
165200050414
165300050418     c                   If        wflg = *Blanks
165400050418     c                   Eval      *In10 = *Off
165500050418     c                   Clear                   waac
165600050418     c                   Clear                   wfil
165700050418     c                   Clear                   wnca
165800050418     c                   Else
165900050418     c                   Eval      *In10 = *On
166000050418     c                   EndIf
166100050411
166200050411     c     *dtaara       define    §azute        azuteds
166300050411     c     *dtaara       define    §datiute      ddatiute
166400050411
166500050411     c                   in(E)     *dtaara
166600050411     c                   IF        %ERROR or RSUT = *blanks
166700050411     c                   clear                   Tibs34Ds
166800050411     c                   call      'TIBS34R'
166900050411     c                   parm                    Tibs34Ds
167000050411     c                   in        *dtaara
167100050411     c                   ENDIF
167200050412
167300050412     c                   Z-add     *date         wdata
167400050414
167500050414     c     kwndc         klist
167600050414     c                   kfld                    waac
167700050414     c                   kfld                    wfil
167800050414     c                   kfld                    wnca
167900050411
168000050411     c     kTitas        Klist
168100050411     c                   Kfld                    dctaas
168200050411     c                   Kfld                    dctlnp
168300050411     c                   Kfld                    dctnrs
168400050411     c                   Kfld                    dctnsp
168500050503
168600050503     c     kTita4        Klist
168700050503     c                   Kfld                    dctaas
168800050503     c                   Kfld                    dctlnp
168900050503     c                   Kfld                    dctnrs
169000050503     c                   Kfld                    dctnsp
169100050503     c                   kfld                    ktrc
169200050411
169300050411     c     kTab          klist
169400050411     c                   kfld                    codut
169500050412     c                   kfld                    ktblcod
169600050412     c                   kfld                    ktblkey
169700050412
169800050412     c     kFndcs        klist
169900050412     c                   kfld                    kdcstpd
170000050412     c                   kfld                    kdcsnkt
170100050412     c                   kfld                    kdcsstd
170200050412     c                   kfld                    kdcsdim
170300050412     c                   kfld                    kdcshim
170400050412     c                   kfld                    kdcsnks
170500050412     c                   kfld                    kdcstrc
170600050415
170700050415     c     kFndcs01      klist
170800050415     c                   kfld                    kdcstpd
170900050415     c                   kfld                    kdcsnkt
171000050415     c                   kfld                    kdcsstd
171100050415     c                   kfld                    kdcsdim
171200050415     c                   kfld                    kdcshim
171300050415     c                   kfld                    kdcsnks
171400050415     c                   kfld                    kdcstrc
171500050415     c                   kfld                    kdcspno
171600050516
171700050516     c     kFndcs02      klist
171800050516     c                   kfld                    kdcstpd
171900050516     c                   kfld                    kdcsnkt
172000050516     c                   kfld                    kdcsstd
172100050412
172200050412     c     kFndet        klist
172300050412     c                   kfld                    dctaae
172400050412     c                   kfld                    dctnev
172500050503
172600050503     c     kFndet2       klist
172700050503     c                   kfld                    dctaad
172800050503     c                   kfld                    dctpod
172900050503     c                   kfld                    dctnde
173000050412
173100050412     c     kFndka        klist
173200050412     c                   kfld                    dctaac
173300050412     c                   kfld                    dctfil
173400050412     c                   kfld                    dctnca
173500050412     c                   kfld                    wtrc
173600050411
173700050414     c     kFndc         klist
173800050413     c                   kfld                    dctaac
173900050413     c                   kfld                    dctfil
174000050413     c                   kfld                    dctnca
174100050414
174200050414     c     kCnabi        klist
174300050414     c                   kfld                    wabi
174400050414     c                   kfld                    wcab
174500050413
174600050411     c                   EndSr
