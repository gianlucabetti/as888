000100070725     h DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200070725     h dftactgrp(*no) actgrp(*caller)
000300070725      * FIOR69R *----------------------------------------------------*
000400070725      *         Assegnaz.distinta Ritiro auto. x giro e sequenza ORM
000500070725      *--------------------------------------------------------------*
000600070913     FFior63p   o    e             printer
000700070725     Ftabel00f  if   e           k disk
000800070725     Fazorg01l  if   e           k disk
000900070725     Ffnorg00f  uf   e             disk    commit
001000070725      *--------------------------------------------------------------*
001100070725      *   SCHIERE
001200070725      *--------------------------------------------------------------*
001300070725     d Errmsg          S             78    DIM(1) CTDATA PERRCD(1)              MSG DI ERRORE
001400070205      *--------------------------------------------------------------*
001500070205      *   DS
001600070205      *--------------------------------------------------------------*
001700070725     d fnorgds       e ds                  extname(fnorg00f)
001800070726     d fnormds       e ds                  extname(fnorm00f)
001900070725     d fidstds       e ds                  extname(fidst00f)
002000070725     d fidstds£      e ds                  extname(fidst00f) prefix(£)
002100070725     d trul82ds      e ds
002200070725     d ddatiute      e ds
002300070725     d azuteds       e ds                  extname(AZUTE00F)
002400070725     d tibs34ds      e ds                  inz
002500070725     d KPJBA         e ds
002600070727     d Fior69ds      e ds
002700070727     d Fior70ds      e ds
002800070725     d OG147         e ds                  INZ
002900071010     d Dorm01        e ds
002901161108     d ddft          e ds
002902161108     d Tibs02ds      e ds
003000070725
003100070725      *--------------------------------------------------------------*
003200070725      *   VARIABILI
003300070725      *--------------------------------------------------------------*
003400070725     drrnorg           s              9  0
003500070725     drrnorm           s              9  0
003600070725     ddtaiso           s               d
003700070725     ddtaeur           s               d   datfmt(*eur)
003800071123     d*** peso            s                   INZ like(orgpkg)
003801071123     d peso            s             14  1 INZ
003900070910     d wpeso           s             14  1 INZ
003901071123     d*** volume          s                   INZ like(orgvlm)
004000071123     d volume          s             10  3 INZ
004100070910     d wvolume         s             10  3 INZ
004101161108     d limitepeso      s              7  1 INZ
004200070725     d savnftl         s                   INZ like(orgnftl)
004300070725     d compkl          s                   INZ like(dstrpkl)
004400070725     d comvlu          s                   INZ like(dstrvlu)
004500070725     d comnftl         s                   INZ like(dstrnftl)
004600070725     d conta           S                   INZ like(dstrnftl)
004700070725     d nftl            S                   INZ like(dstrnftl)
004800070725     d prima           S              1
004900070725     d tolle           S              1
005000070725     d oktoll          S              1
005100070725      *--------------------------------------------------------------*
005200070725      *  Ciclo principale
005300070725      *--------------------------------------------------------------*
005400070725     c                   clear                   savnftl
005500070725     c                   clear                   conta
005600070725     c                   clear                   prima
005700070725     c                   clear                   tolle
005800070725      * per prima cosa imposto la seq. di lettura degli orm  e la fermata
005900070725     c                   clear                   fior70ds
006000070725     c                   eval      or70fgsi = or69fgsi
006100070725     c                   eval      or70npgi = or69npgi
006200070725     c                   eval      or70dfvi = or69dfvi
006300070725     c                   eval      or70nfvi = or69nfvi
006400070725     c                   eval      or70pdri = or69pdri
006500070725     c                   eval      or70fgsgi= or69fgsgi
006600070725     c                   eval      or70cgii = or69cgii
006700070725     c                   eval      or70rpkli =or69rpkli
006800070725     c                   eval      or70rvlui =or69rvlui
006900070725     c                   eval      or70rnftli=or69rnftli
007000070725     c                   eval      or70pag= pages
007100070725     c                   eval      or70riga= RIGA
007200070725     C                   eval      kpjbu = Fior70DS
007300070725     c                   call      'FIOR70R'
007400070725     c                   parm                    kpjba
007500070727     c                   eval      fior70ds = kpjbu
007600070725     c                   eval      pages = or70pag
007700070725     c                   eval      riga  = or70riga
007800070725      * se ritorno con errore non proseguo
007900070725     c                   if        or70erro <> ' '
008000070726     c                   exsr      srfine
008100070726     c                   end
008200070725      *
008300070725      * se il p.o. è in parallelo testo la distinta su ORG altrimenti
008400070726      * se il p.o. è partito testo la distinta anche su ORM
008500070725      *
008600070725      * per il momento aggiorno solo ORG perchè ORM/ORF li aggiornerò alla fine
008700070725      *
008800070725      * se la sequenza ORM é = 0 l'ORM non è da elaborare
008900070725      *
009000070725     c                   if        comdda > dateu8
009100070725     C/EXEC SQL
009200070725     C+ DECLARE A1 CURSOR FOR SELECT rrn(fnorg01l), rrn(fnorm01l),
009300070725     C+ fnorg01l.*, fnorm01l.*, orgnftl
009400070725     C+ FROM fnorg01l join fnorm01l on ormpoe=orgpoe
009500070725     C+ and ormnsr=orgnsr and ormnor=orgnor and ormnrv=orgnrv
009600070725     C+ WHERE orgndc = 0 and orgcgi=:or69cgii and orgslo <> 0 and
009700070725     C+ orgpocgi = :or69fgsi  ORDER BY
009800070919     C+ Fnorg01l.orgslo desc, Fnorg01l.orgnftl
009900070725     C/END-EXEC
010000070725
010100070725     C/EXEC SQL
010200070725     C+ OPEN a1
010300070725     C/END-EXEC
010400070725     c                   else
010500070725     c* oltre a ORMNDC = 0 testo anche ORGNDC=0 per evitare di rielaborare
010600070801     c* gli ORM  già inseriti nelle precedenti distinte
010700070725     C/EXEC SQL
010800070725     C+ DECLARE B1 CURSOR FOR SELECT rrn(fnorg01l), rrn(fnorm01l),
010900070725     C+ fnorg01l.*, fnorm01l.*, orgnftl
011000070725     C+ FROM fnorg01l join fnorm01l on ormpoe=orgpoe
011100070725     C+ and ormnsr=orgnsr and ormnor=orgnor and ormnrv=orgnrv
011200070725     C+ WHERE orgndc = 0 and orgcgi=:or69cgii and orgslo <> 0 and
011300071105     C+ orgpocgi = :or69fgsi and ormndc = 0 and ormeti <> ' ' ORDER BY
011400070919     C+ Fnorg01l.orgslo desc, Fnorg01l.orgnftl
011500070725     C/END-EXEC
011600070725
011700070725     C/EXEC SQL
011800070725     C+ OPEN b1
011900070725     C/END-EXEC
012000070725     c                   end
012100070725
012200070725     C                   do        *hival
012300061003
012400070725     c                   if        comdda > dateu8
012500061003     C/EXEC SQL
012600070725     C+ FETCH NEXT FROM a1 INTO :rrnorg, :rrnorm, :fnorgds, :fnormds, :nftl
012700061003     C/END-EXEC
012800070205     c                   else
012900070205     C/EXEC SQL
013000070725     C+ FETCH NEXT FROM b1 INTO :rrnorg, :rrnorm, :fnorgds, :fnormds, :nftl
013100070205     C/END-EXEC
013200070205     c                   end
013300070725
013400070725     c                   select
013500070725     c                   when      sqlcod = 100
013600070725     c                   leave
013700070725     c                   WHEN      SqlCod < 0
013800070725     c                   if        sqlcod <> -913
013900070725     c                   seton                                        H1
014000070725     c                   end
014100070725     c                   seton                                        83
014200070725     c                   exsr      srstampa
014300070725     c                   exsr      srfine
014400070725     c                   other
014500070725     c                   exsr      gesorm
014600070725      * se sono a capienza esco
014700070725     c                   if        prima = '1'
014800070725     c                   leave
014900070725     c                   end
015000070725     c                   endsl
015100070725
015200070725     c                   enddo
015300061003
015400070725     c                   if        comdda > dateu8
015500070725     C/EXEC SQL
015600070725     C+ CLOSE a1
015700070725     C/END-EXEC
015800070725     c                   else
015900070725     C/EXEC SQL
016000070725     C+ CLOSE b1
016100070725     C/END-EXEC
016200070725     c                   end
016300070725     C*
016400070725     c                   if        or69cmti= 'S'
016500070206     c                   commit
016600070206     c                   end
016700070725     c                   eval      or69pag = pages
016800070725     c                   eval      or69riga= riga
016900070725
017000070725     c                   eval      kpjbu = fior69ds
017100070725
017200070725     c                   SETON                                        LR
017300070725      *--------------------------------------------------------------*
017400070725      *  Gestione singolo ORM
017500070725      *--------------------------------------------------------------*
017600070725     c     GESorm        BEGSR
017700070725      * controllo che l'ORM non raggiunga i limiti di capienza
017800070725      *
017900070725     c                   exsr      srctrorm
018000070214     c                   if        not *in28
018100070726      * aggiorno sempre ORG
018200070726     c     RRNorg        chain(e)  fnorg00f
018300070206     c                   if        %error
018400070523     c                   exsr      geslck
018500070523     c                   if        ul82§sts = 'A'
018600070704     c                   seton                                        85
018700070726     c                   eval      prtmsg = 'L''ORM non è stata aggiorna-
018800070726     c                             to correttamente.VERIFICARLO'
018900070222     c                   exsr      srstampa
019000070206     c                   leavesr
019100070528     c                   else
019200070726     c     RRNorg        chain     fnorg00f
019300070206     c                   end
019400070523     c                   end
019500070726      *
019600070726     c                   if        not %found(fnorg00f)
019700070222     c                   seton                                        84
019800070222     c                   exsr      srstampa
019900070206     c                   else
020000070726      * se non ci sono errori aggiorno org
020100070726     c                   eval      orgpdc = or69pdri
020200070726     c                   eval      orgFGS = or69fgsi
020300070726     c                   eval      orgNDC = or69nfvi
020400070726     c                   eval      orgDDC = or69dfvi
020500070726     c                   eval      orgDTVDIS = dateu8
020600070726     c                   eval      orgHVDIS= ora
020700070726     c                   update    fnorg000
020800070910     c     or69rpklo     add       orgpkg        wpeso
020900070910     c                   If        Wpeso >  999999,9
021000070910     c                   eval      or69rpklo = 999999,9
021100070910     c                   else
021200070726     c                   eval      or69rPKLo = or69rpklo + orgpkg
021300070910     c                   endif
021400070910     c     or69rvluo     add       orgvlm        wvolume
021500070910     c                   If        Wvolume >  99,999
021600070910     c                   eval      or69rvluo = 99,999
021700070910     c                   else
021800070726     c                   eval      or69rVLUo = or69rvluo + orgvlm
021900070910     c                   endif
022000070726     c                   eval      or69rnftlo= conta
022100070726     c                   eval      or69rnblo = or69rnblo + 1
022200070727     c                   eval      or69rnbao = or69rnbao + ormbnc
022300070222     c                   end
022400070227     c                   end
022500070726
022600070726     c                   ENDSR
022700070302      *-----------------------------------------------
022800070726     c     srctrorm      begsr
022900070302      *-----------------------------------------------
023000070202     c                   setoff                                       28
023100071010      * controllo se richiesta una selezione sugli orm con allegati
023200071010     c                   if        or69allega <> ' '
023300071010     c                   movel     ormflo        dorm01
023400071010     c                   if        or69allega = 'E' and (§orsrm = 'S' or
023500071010     c                             ormtor = 'P')
023600071010     c                   seton                                        2885
023700071010     c                   eval      prtmsg = 'ORM con allegati esclusi da video'
023800071010     c                   exsr      srstampa
023900071010     c                   leavesr
024000071010     c                   endif
024100071010     c                   if        or69allega = 'I' and (§orsrm = ' ' and
024200071010     c                             ormtor <> 'P')
024300071010     c                   seton                                        2885
024400071010     c                   seton                                        90
024500071010     c                   eval      prtmsg = 'ORM no  allegati inclusi a video'
024600071010     c                   exsr      srstampa
024700071010     c                   leavesr
024800071010     c                   endif
024900071010     c                   endif
024901161108      * controllo se richiesta una selezione sugli orm con peso fino a xxx kg
024902161108     c                   if        or69finoa <> ' '
024903161109
024904161110     c                   if        or69finoa  = 'E' and  orgpkg <  limitepeso
024906161108     c                   seton                                        2885
024907161110     c                   eval      prtmsg = 'ORM con peso < limite Esclusi +
024908161110     c                             da Video'
024909161109     c                   exsr      srstampa
024910161108     c                   leavesr
024911161108     c                   endif
024912161110     c                   if        or69finoa  = 'I' and orgpkg >= limitepeso
024914161108     c                   seton                                        2885
024915161108     c                   seton                                        90
024916161110     c                   eval      prtmsg = 'ORM con peso >= limite Inclusi +
024917161109     c                             a Video'
024918161108     c                   exsr      srstampa
024919161108     c                   leavesr
024920161108     c                   endif
024921161108     c                   endif
025000070726      * controllo incassi
025100071012     c                   if        dstrinc ='N'
025200070726      * controllo il tipo ORM è un prepagato
025300070726
025400070726     c                   if        ormtor = 'P'
025500070328     c                   seton                                        2885
025600071010     c                   seton                                        90
025700070726     c                   eval      prtmsg = 'ORM prepagato ma in distinta no'
025800070328     c                   exsr      srstampa
025900070206     c                   leavesr
026000070206     c                   end
026100070726     c                   end
026200070911      * controllo sponda idraulica
026300070911     c                   if        dstrspi = 'N'
026400071010      * controllo il flag sponda idraulica negli ORM
026500070911
026600070911     c                   if        ormspi = 'S'
026700070911     c                   seton                                        2885
026800070911     c                   seton                                        90
026900070911     c                   eval      prtmsg = 'ORM con sponda idraulica ma in di-
027000070911     c                             stinta NO'
027100070911     c                   exsr      srstampa
027200070911     c                   leavesr
027300070911     c                   end
027400070911     c                   end
027500070911     c
027600070726      * ROTTURA di fermata
027700070726    1c                   if        orgnftl <> savnftl
027800070726      * se ho già dato una fermata utilizzando la tolleranza esco
027900070726    2c                   if        tolle = '1'
028000070726     c                   seton                                        2885
028100070330     c                   seton                                        90
028200070726     c                   eval      prtmsg = 'Primo ORM-fermata che eccede per -
028300070330     c                             peso o volume'
028400070330     c                   exsr      srstampa
028500070330     c                   eval      prima = '1'
028600070330     c                   leavesr
028700070726    2c                   end
028800070726      * controllo la capienza A ROTTURA di fermata
028900070726     c                   exsr      srecce
029000070925    1c****               end
029100070726
029200070726    1c                   if        (peso > (or69rpkli - or69rpklo) or
029300070726     c                             volume > (or69rvlui - or69rvluo))
029400070726      * provo con la tolleranza
029500070726    2c                   if        dstrtol <> 0
029600070726      * testo il peso e il volume solo se non ho già attribuito una o +
029700070726      * ORM tocciando dalla tolleranza. in questo caso finisco
029800070801      * di attribuire gli ORM    con stessa fermata
029900070726    3c                   if        tolle = ' '
030000070726    4c                   if        peso > (compkl + (or69rpkli - or69rpklo)) or
030100070726     c                             volume > (comvlu + (or69rvlui - or69rvluo))
030200070726     c                   seton                                        2885
030300070726     c                   seton                                        90
030400070726     c                   eval      prtmsg = 'Primo ORM-fermata che eccede per -
030500070726     c                             peso o volume'
030600070726     c                   exsr      srstampa
030700070726     c                   leavesr
030800070726     c                   else
030900070726     c                   eval      TOLLE = '1'
031000070726    4c                   end
031100070726    3c                   end
031200070726     c                   else
031300070726     c                   seton                                        2885
031400070726     c                   seton                                        90
031500070726     c                   eval      prtmsg = 'Primo ORM-fermata che eccede per -
031600070726     c                             peso o volume'
031700070726     c                   exsr      srstampa
031800070726     c                   leavesr
031900070726    2c                   end
032000070726    1c                   end
032100070726      * controllo le fermate
032200070925     c****               if        orgnftl <> savnftl
032300070726     c                   add       1             conta
032400070925     c****               end
032500070726     c                   if        conta> or69rnftli
032600070726     c                   seton                                        2885
032700070726     c                   seton                                        90
032800070726     c                   eval      prtmsg = 'L''ORM eccede per numero fe-
032900070726     c                             rmate'
033000070726     c                   exsr      srstampa
033100070726     c                   eval      prima = '1'
033200070726     c                   leavesr
033300070726     c                   end
033400070925     c                   end
033500070726     c                   eval      savnftl = orgnftl
033600070726
033700070726     c                   ENDSR
033800070726      *--------------------------------------------------------------*
033900070726      * verifico peso volume x fermata
034000070726      *--------------------------------------------------------------*
034100070726     c     srecce        begsr
034200070726     c                   clear                   peso
034300070726     c                   clear                   volume
034400071123     c**                 clear                   wpeso
034500071123     c**                 clear                   wvolume
034600070726     c                   if        comdda > dateu8
034700070726     C/EXEC SQL
034800070726     C+ DECLARE C1 CURSOR FOR SELECT fnorg01l.* FROM fnorg01l join
034900070726     C+ fnorm01l on ormpoe= orgpoe and ormnsr=orgnsr and ormnor= orgnor
035000070726     C+ and ormnrv= orgnrv WHERE orgndc = 0 and orgcgi=:or69cgii and
035100070726     C+ orgslo <> 0 and orgpocgi = :or69fgsi and orgnftl = :nftl
035200070726     C/END-EXEC
035300070726
035400070726     C/EXEC SQL
035500070726     C+ OPEN c1
035600070726     C/END-EXEC
035700070726     c                   else
035800070726     C/EXEC SQL
035900070726     C+ DECLARE D1 CURSOR FOR SELECT fnorg01l.* FROM fnorg01l join
036000070726     C+ fnorm01l on ormpoe= orgpoe and ormnsr=orgnsr and ormnor= orgnor
036100070726     C+ and ormnrv= orgnrv WHERE orgndc = 0 and orgcgi=:or69cgii and
036200070726     C+ orgslo <> 0 and orgpocgi = :or69fgsi and orgnftl = :nftl
036300071105     C+ and ormndc = 0 and ormeti <> ' '
036301071105     C+ ORDER BY FNORG01L.ORGslo desc, FNORG01L.ORGnftl
036400070726     C/END-EXEC
036500070726
036600070726     C/EXEC SQL
036700070726     C+ OPEN d1
036800070726     C/END-EXEC
036900070726     c                   end
037000070726
037100070726     C                   do        *hival
037200070726
037300070726     c                   if        comdda > dateu8
037400070726     C/EXEC SQL
037500070726     C+ FETCH NEXT FROM c1 INTO :fnorgds
037600070726     C/END-EXEC
037700070726     c                   else
037800070726     C/EXEC SQL
037900070726     C+ FETCH NEXT FROM d1 INTO :fnorgds
038000070726     C/END-EXEC
038100070726     c                   end
038200070726
038300070726     C                   select
038400070726     C                   when      sqlcod = 100
038500070726     c                   leave
038600070726     C                   WHEN      SqlCod < 0
038700070726     c                   if        sqlcod <> -913
038800070726     C                   seton                                        H1
038900070726     c                   end
039000070726     c                   seton                                        83
039100070302     c                   exsr      srstampa
039200070726     c                   exsr      srfine
039300070726     C                   other
039301071123     c                   add       orgpkg        peso
039400071123     c*    peso          add       orgpkg        wpeso
039500071123     c*                  if        wpeso > 999999,9
039600071123     c*                  eval      peso = 999999,9
039700071123     c*                  else
039800071123     c*                  eval      peso = wpeso
039900071123     c*                  endif
040000070910      *
040001071123     c                   add       orgvlm        volume
040100071123     c*    volume        add       orgvlm        wvolume
040200071123     c*                  if        wvolume > 99,999
040300071123     c*                  eval      volume = 99,999
040400071123     c*                  else
040500071123     c*                  eval      volume = wvolume
040600071123     c*                  endif
040700070726     C                   endsl
040800070726     C*
040900070726     C                   enddo
041000070726
041100070726     c                   if        comdda > dateu8
041200070726     C/EXEC SQL
041300070726     C+ CLOSE c1
041400070726     C/END-EXEC
041500070726     c                   else
041600070726     C/EXEC SQL
041700070726     C+ CLOSE d1
041800070726     C/END-EXEC
041900070726     c                   end
042000070726     c                   ENDSR
042100070726      *--------------------------------------------------------------*
042200070726      * calcola la capienza
042300070726      *--------------------------------------------------------------*
042400070726     c     srtol         BEGSR
042500070726
042600070214     c                   clear                   compkl
042700070214     c                   clear                   comvlu
042800070214     c                   clear                   comnftl
042900070726
043000070726     c                   if        dstrtol <> 0
043001071123      * se i valori di assegnabilità in distinta sono già al massimo non calcolo
043002071123      * la tolleranza
043003071123     c                   if        dstrpkl <> *hival
043100070914     c                   eval      compkl = ((dstrpkl * dstrtol)/ 100)
043101071123     c                   endif
043102071123     c                   if        dstrvlu <> *hival
043200070914     c                   eval      comvlu = ((dstrvlu * dstrtol)/ 100)
043201071123     c                   endif
043202071123     c                   if        dstrnftl <> *hival
043300070914     c                   eval      comnftl= ((dstrnftl * dstrtol)/ 100)
043301071123     c                   endif
043302071123
043400070214     c                   end
043500070727     c                   ENDSR
043501161108      *--------------------------------------------------------------*
043502161108      *  Recupero dati dalla tabella DFT (peso limite)
043503161108      *--------------------------------------------------------------*
043504161108     c     Rec_dft       Begsr
043505161108
043506161108      * Recupero i dati di default prima con il mio p.o.
043507161108     C                   clear                   ddft
043508161108     C                   clear                   TIBS02DS
043509161108     C                   movel     'C'           T02mod
043510161108     C                   movel     knsif         t02sif
043511161108     C                   movel     'DFT'         t02cod
043512161108     C                   movel(p)  or69fgsi      T02ke1
043513161108     C                   call      'TIBS02R'
043514161108     C                   parm                    KPJBA
043515161108     C                   parm                    TIBS02DS
043516161108     C                   if        t02err =  *blanks
043517161108     C                   movel     t02uni        ddft
043518161108     C                   else
043519161108      * poi con 999 generico se non trovo con il mio p.o.
043520161108     C                   clear                   TIBS02DS
043521161108     C                   movel     'C'           T02mod
043522161108     C                   movel     knsif         t02sif
043523161108     C                   movel     'DFT'         t02cod
043524161108     C                   movel(p)  '999'         T02ke1
043525161108     C                   call      'TIBS02R'
043526161108     C                   parm                    KPJBA
043527161108     C                   parm                    TIBS02DS
043528161108     C                   if        t02err =  *blanks
043529161108     C                   movel     t02uni        ddft
043530161108     C                   endif
043531161108     C                   endif
043532161108      * valorizzo il campo kg limite
043533161108     c                   z-add     D§DFTPKGDT    Limitepeso
043534161108     c                   if        limitepeso = 0
043535161108     c                   z-add     9999999,9     limitepeso
043536161108     c                   endif
043537161108
043538161108     c                   Endsr
043600070727      *------------------------------------------------------------------------*
043700070727      * stampa
043800070727      *------------------------------------------------------------------------*
043900070727     c     srstampa      BEGSR
044000070727     c                   move      or69dfvi      dtaiso
044100070222     c                   move      dtaiso        dtaeur
044200070222     c                   move      dtaeur        prtdfv
044300070727      * PAGINA IN STAMPA
044400070323     C                   IF        *IN80 OR RIGA >=60
044500070323     C                   SETON                                        80
044600070323     C                   ADD       1             PAGES
044700070323     C                   Z-ADD     3             RIGA              3 0
044800070323     C                   END
044900070727
045000070328     c                   if        *in85
045100070727     c                   eval      prtpoe = ormpoe
045200070727     c                   eval      prtnsr = ormnsr
045300070727     c                   eval      prtnor = ormnor
045400070727     c                   eval      prtnrv = ormnrv
045500070727     c                   eval      prtnfv = or69nfvi
045600070727     c                   eval      prtcgi = or69cgii
045700070328     c                   end
045800070328     c                   if        *in86
045900070328     c                   eval      prtpesb= peso
046000070328     c                   eval      prtvolb= volume
046100070727     c                   if        dstrtol <> 0
046200070727     c                   eval      prtpesd = (compkl + (or69rpkli - or69rpklo))
046300070727     c                   eval      prtvold = (comvlu + (or69rvlui - or69rvluo))
046400070329     c                   else
046500070727     c                   eval      prtpesd = (or69rpkli - or69rpklo)
046600070727     c                   eval      prtvold = (or69rvlui - or69rvluo)
046700070328     c                   end
046800070329     c                   end
046900070727      *
047000070222     c   80              write     testa
047100070222     c                   setoff                                       80
047200070222     c   83              write     riga3
047300070323     C   83              ADD       1             RIGA
047400070222     c   84              write     riga4
047500070323     C   84              ADD       1             RIGA
047600070328     c   85              write     riga5
047700070328     C   85              ADD       1             RIGA
047800070328     c   86              write     riga6
047900070328     C   86              ADD       1             RIGA
048000070328     c                   setoff                                       838485
048100070328     c                   setoff                                       86
048200070727      *
048300070222     c                   endsr
048400070726      *--------------------------------------------------------------*
048500070726      *  Operazioni iniziali
048600070726      *--------------------------------------------------------------*
048700070726     c     *INZSR        BEGSR
048800070726
048900070726     c     *ENTRY        PLIST
049000070726     c                   PARM                    KPJBA
049100070726     c                   PARM                    fidstds£
049200070726     c                   MOVEL(P)  KPJBU         Fior69ds
049300070726     c                   eval      pages = or69pag
049400070726     c                   eval      riga = or69riga
049500070302     c                   eval      fidstds = fidstds£
049600070726     c                   clear                   or69erro
049700070726
049800030718     c     *dtaara       define    §azute        azuteds
049900030718     c     *dtaara       define    §datiute      ddatiute
050000070726     c                   in(E)     *dtaara
050100070726     c                   IF        %Error  or  RSUT = *blanks
050200070726     c                   call      'TIBS34R'
050300070726     c                   parm                    Tibs34Ds
050400070726     c                   in        *dtaara
050500030718     c                   ENDIF
050600070726
050700070726     c                   TIME                    WHHUDT           14 0
050800070726     c                   MOVEL     WHHUDT        ORA               6 0
050900070913     c                   MOVE      WHHUDT        UDATE8            8 0
051000070913     c                   TIME                    WHDAT            12 0
051100070913     c                   MOVE      WHDAT         WOGGI             6 0
051200070726     c                   move      udate8        dtaeur
051300070510     c                   move      dtaeur        dtaiso
051400070510      *
051500070206     c                   move      dtaiso        dateu8            8 0
051600070726     c     or69fgsi      CHAIN     AZORG01l
051700070726    1c                   IF        %found(azorg01l)
051800070726     c                   MOVEL     ORGde7        og147
051900070726      * se data distinte automatiche < oggi p.o. partito
052000070726     c                   if        §ogddao = ' '
052100070205     c                   exsr      srfine
052200070205     c                   else
052300070726     c                   move      §ogddao       comdda            8 0
052400070205     c                   end
052500070205     c                   end
052501161108
052502161108     c* recupero i pesi limite ORM per ritiri con consegne
052503161108     c                   exsr      Rec_dft
052504161108
052600950525     C*  Definisco chiavi di accesso
052700070206     c*
052800070209     c                   exsr      srtol
052900070726
053000950525     C                   ENDSR
053100070726      *--------------------------------------------------------------*
053200070726      *  Operazioni finali in caso di errore
053300070726      *--------------------------------------------------------------*
053400070726     c     srfine        BEGSR
053500070726     c                   if        or69cmti= 'S'
053600070205     c                   rolbk
053700070205     c                   end
053800070726     c                   eval      or69erro= '1'
053900070726     c                   eval      kpjbu = fior69ds
054000070205     c                   seton                                        lr
054100070205     c                   return
054200070205     c                   endsr
054300070726      *----------------------------------------------------*
054400070523     C     geslck        begsr
054500070726      *----------------------------------------------------*
054600070523     c                   clear                   trul82ds
054700070727     c                   Eval      ul82§rrn = rrnorg
054800070726     c                   Eval      UL82§FIL = 'FNORG00F'
054900070523     c                   Eval      UL82§WIN = 'S'
055000070523     c                   Eval      UL82§num = 2
055100070523     c                   Eval      UL82§att = 2
055200070523     c                   eval      ul82§mss = ErrMsg(1)
055300070726     c                   Eval      UL82§msw = 'L''ORM '
055400070726     c                             + %editc(orgpoe:'Z') +  ' '
055500070726     c                             + %editc(orgnsr:'Z') +  ' '
055600070726     c                             + %editc(orgnor:'Z') +  ' '
055700070726     c                             + %editc(orgnrv:'Z') +  ' è già utilizzato -
055800070726     c                             da un altro lavoro e non è manutenzionabile'
055900070523      * Effettuo la chiamata al *PGM d utilità
056000070523     c                   Call(e)   'TRUL82R'
056100070523     c                   Parm                    Trul82ds
056200070523
056300070523     C                   ENDSR
056400070523** ErrMsg
056500070523Si sta bloccando l'assegnazione DISTINTE: SI PREGA DI USCIRE dal lavoro!       27
