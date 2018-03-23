000100080731     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200080731      // ----------------------------------------------------------------------
000300080725      //
000400110617      //         MONITOR PER attività PDA DETTAGLIO  ?
000500080725      //
000600080731      // ----------------------------------------------------------------------
000700080725      // ? DICHIARAZIONE DEI FILE ?
000800080731      // ----------------------------------------------------------------------
000900070209
001000110624     ftntbe01l  if   e           k disk
001100110624     ftabel00f  if   e           k disk
001101131126     ffipct02l  if   e           k disk
001102140213     ffipro11l  if   e           k disk
001200110617     ffnlvp0d2  cf   e             workstn sfile(lvp0s01:nrr)
001300070209
001400080725      // ? V A R I A B I L I ?
001500080804     d nrr             s                   like(recsf1)
001600080725     d xx              s              3  0
001700110623     d ora             s              6
001800080731     d $fine           s               n
001900080806     d $finearb        s               n
002000080806     d $fineorg        s               n
002100110705     d $fineorgm       s               n
002200121204     d flg_errdata     s              1
002300070209
002400080725      // ? S C H I E R E ?
002500101124     d msg             s             78    dim(15) ctdata perrcd(1)
002600070209
002700080725      // ? D S   I N T E R N E / E S T E R N E ?
002800080806     d p07ds           ds
002900080806     d  p07poe                        3  0
003000080806     d  p07nor                        7  0
003100080806     d  p07nsr                        2  0
003200080806     d  p07nrv                        2  0
003300080806     d  p07sce                        1
003400080806     d  p07fgs                        3  0
003500080806     d  p07por                        3  0
003600080806     d  p07dtr                        8  0
003700080806     d  p07mdb                       10
003800080806     d  p07prb                       10
003900080806     d  p07dts                        8  0
004000080806     d  p07rmp                        1
004100080806     d  p07brc                        1
004200080806     d  p07ref                        2
004300080806     d  p07mio                        1
004400080806     d  p07dtv                        8  0
004500080806     d  p07orv                        6  0
004600080806     d  p07utv                       10
004700080806     d  p07ric                        1
004800080806
004900080806     d p36ds           ds                  inz
005000080806     d  p36aas                14     17  0
005100080806     d  p36lnp                18     20  0
005200080806     d  p36nrs                21     22  0
005300080806     d  p36nsp                23     29  0
005400080806     d  p36f03                30     30
005500080806     d  p36flg                31     31
005600080806
005700080725     d wlbdat          ds                  inz
005800080725     d  g02dat                 1      8  0
005900080725     d  g02inv                 9     16  0
006000080725     d  g02err                17     17
006100080725     d  g02tgi                18     22  0
006200080806
006300080806     d §azute        e ds                  extname(azute00f)
006400080806     d                                     dtaara
006500080806     d §datiute      e ds                  extname(ddatiute)
006600080806     d                                     dtaara
006700110623     d FIPRCRESDS    e ds
006800110623     d FIPRdcetDS    e ds
006801140124     d FipctCORds    e ds
006802140213     d FiproRORds    e ds
006900121102     d WRKDATHHMM      s             12A
007000121102     D Error_DATA      C                   CONST('DATA < o > di 10 GG')
007100110623     d fiapdds       e ds                  extname(fiapd00f)
007200080729     d fidstds       e ds                  extname(fidst00f)
007300110623     d fiplgds       e ds                  extname(fiplg00f)
007400080805     d fnarbds       e ds                  extname(fnarb00f)
007500110629     d fnorgds       e ds                  extname(fnorg00f)
007600110629     d fnormds       e ds                  extname(fnorm00f)
007700110624     d ds2a          e ds
007800110624     d dcmr          e ds
007900080805     d fnlvp0ds      e ds
008000070209     d kpjba         e ds
008001131126     d fidg46ds      e ds
008100080806     d tibs34ds      e ds                  inz
008200110217
008300070209     d                sds
008400070209     d  vtcpgm                 1     10
008500080801
008600080801      // ? Ordinamento SBFL ?
008700080801      // ----------------------------------------------------------------------
008800080801      //?Costanti per ordinamento subfile
008900080801      // ----------------------------------------------------------------------
009000140218     d MaxKey          c                   3
009100080801     d Ascendente      c                   1
009200080801     d Discendente     c                   2
009300080801     d Carattere       c                   6
009400080801     d Numerico        c                   8
009500080801     d Put             c                   1
009600080801     d EndPut          c                   2
009700080801     d Get             c                   3
009800080801      **********************************************************************
009900080801      * Campi utili:
010000080801      *     nrr        - Numero relativo di record del Subfile
010100080801      *     SizeList   - Dimensione della lista
010200080801      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
010300080801      **********************************************************************
010400080801     d NotUsed         s             16A
010500080801     d ReturnSize      s              9B 0
010600080801     d SizeList        s              9B 0
010700080801     d RrnLast         s              5I 0
010800080801     d WrkSort         s              1  0 inz(0)
010900080801      **********************************************************************
011000080801      * Data Structures
011100080801      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
011200080801      *     QLGSCB     - The sort request block for the QLGSORT API
011300080801      *     QLGSCB00   - The sort request block for the QLGSRTIO API
011400080801      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
011500080801      *     QUSEC      - Error structure for the QLGSORT API
011600080801      **********************************************************************
011700080801     d sflrcd          ds
011800110623     d  v1stipo
011900080805     d  v1srag
012000080805     d  v1sind
012100080805     d  v1sloc
012200080805     d  v1spkg
012300080805     d  v1svlm
012400110623     d  v1scmc
012500110623     d  v1sora
012501140124     d  v1hmisto
012600110624     d  v1dcmc
012700080807     d  v1saas
012800080807     d  v1slnp
012900080807     d  v1snrs
013000080807     d  v1snsp
013100110623     d  v1spoe
013200110623     d  v1snsr
013300110623     d  v1snor
013400110623     d  v1snrv
013500120528     d  v1stsp
013600110707     d  v1hman
013601131127     d  v1scep
013603140124     d  v1soras
013604140124     d  v1sord
013700080807     d  selected                      1A
013800080801
013900080801      /COPY QSYSINC/QRPGLESRC,QLGSORT
014000080801     d QLGKL                         16    DIM(MaxKey)
014100080801     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
014200080801     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
014300080801     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
014400080801     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
014500080801
014600080801      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
014700080801      /COPY QSYSINC/QRPGLESRC,QUSEC
014800080801
014900080725
015000080728      // ? PROTOTIPI ?
015100080806      /copy gaitrasrc/srcprotopr,fior07r
015200080806      /copy gaitrasrc/srcprotopr,fnlr36r
015300080806      /copy gaitrasrc/srcprotopr,tibs34r
015400080728      /copy gaitrasrc/srcprotopr,xsrda8
015401131126      /copy gaitrasrc/srcprotopr,fidg49r
015500080801
015600080801     d Qlgsort_pr      pr                  extpgm('QLGSORT')
015700080801     d  pr_QLGSCB                          like(Qlgscb)
015800080801     d  pr_NotUsed1                        like(NotUsed)
015900080801     d  pr_NotUsed2                        like(NotUsed)
016000080801     d  pr_SizeList                        like(SizeList)
016100080801     d  pr_ReturnSize                      like(ReturnSize)
016200080801     d  pr_QUSEC                           like(Qusec)
016300080801     d                                     options(*varsize)
016400080801
016500080801     d Qlgsrtio_pr     pr                  extpgm('QLGSRTIO')
016600080801     d  pr_QLGSCB00                        like(QLGSCB00)
016700080801     d  pr_SflRcd                          like(SflRcd)
016800080801     d  pr_NotUsed1                        like(NotUsed)
016900080801     d  pr_SizeList                        like(SizeList)
017000080801     d  pr_NotUsed2                        like(NotUsed)
017100080801     d  pr_QUSEC                           like(Qusec)
017200080801     d                                     options(*varsize)
017300080801
017400080801     d Qlgsrtio_pr2    pr                  extpgm('QLGSRTIO')
017500080801     d  pr_QLGSCB00                        like(QLGSCB00)
017600080801     d  pr_NotUsed1                        like(NotUsed)
017700080801     d  pr_SflRcd                          like(SflRcd)
017800080801     d  pr_SizeList                        like(SizeList)
017900080801     d  pr_NotUsed2                        like(NotUsed)
018000080801     d  pr_QUSEC                           like(Qusec)
018100080801     d                                     options(*varsize)
018200070209
018300080725      // ----------------------------------------------------------------------
018400080725
018500080725     c     *entry        plist
018600080725     c                   parm                    kpjba
018700080804     c                   parm                    fnlvp0ds
018800110624     c     Ktab          klist
018900110624     c                   kfld                    tblkut
019000110624     c                   kfld                    tblcod
019100110624     c                   kfld                    tblkey
019200110624     c                   eval      tblkut = 1
019300110624     c                   eval      tblcod = '2A'
019400110624     c     Ktbe          klist
019500110624     c                   kfld                    tbecod
019600110624     c                   kfld                    tbeke1
019700110624     c                   eval      tbecod = 'CMR'
019701131126     c     kpct          klist
019703131127     c                   kfld                    §CETFGS
019704131127     c                   kfld                    §CETNDC
019705131127     c                   kfld                    dstpdr
019706131127     c                   kfld                    tiporcd           3
019707131127     c                   kfld                    §CETAAS
019708131127     c                   kfld                    §CETLNP
019709131127     c                   kfld                    §CETNRS
019710131127     c                   kfld                    §CETNSP
019711131126     c                   move      'CEP'         tiporcd
019712140124     c     kpctcor       klist
019713140211     c                   kfld                    dstFGS
019714140211     c                   kfld                    dstNFV
019715140124     c                   kfld                    dstpdr
019716140124     c                   kfld                    tipocor           3
019717140211     c                   kfld                    arbAAS
019718140211     c                   kfld                    arbLNP
019719140211     c                   kfld                    arbNRS
019720140211     c                   kfld                    arbNSP
019721140124     c                   move      'COR'         tipocor
019722140213     c     kproROR       klist
019723140213     c                   kfld                    dstFGS
019724140213     c                   kfld                    dstNFV
019725140213     c                   kfld                    tipoROR           3
019726140213     c                   kfld                    dstpdr
019727140213     c                   kfld                    orgpoe
019728140213     c                   kfld                    orgnsr
019729140213     c                   kfld                    orgnor
019730140213     c                   kfld                    orgnrv
019731140213     c                   move      'ROR'         tipoROR
019800071217
019900080725      /free
020000080725
020100080731       //?operazioni iniziali
020200080725         exsr routinz;
020300080725
020400080729         dow not $fine;
020500080729
020600080805       //?gestione videata dettaglio
020700080805          exsr gess01;
020800080801
020900080731         enddo;
021000080729
021100080806         clear kpjbu;
021200080729         *inlr = *on;
021300080725
021400080731       // ----------------------------------------------------------------------
021500080731       //?Operazioni iniziali.
021600080731       // ----------------------------------------------------------------------
021700080725       begsr routinz;
021800080725
021900080725         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
022000080725
022100080804       //?controllo se passati tutti i parametri
022200080804         if %parms = 1;
022300080804          clear fnlvp0ds;
022400080804         endif;
022500080806
022600080806       //?recupero dei dati utente
022700080806          in(e) §azute;
022800080806          if not %error;
022900080806           in(e) §datiute;
023000080806          endif;
023100080806          if %error or rsut = *blanks;
023200080806           tibs34r(tibs34ds);
023300080806           in §azute;
023400080806           in §datiute;
023500080806          endif;
023600080805
023700080729         $fine =*off;
023800080806
023900080806       //?carico i dati relativi alla distinta passata
024000110623         *in12=*on;
024100080806         exsr sr_carica;
024200080725
024300080725       endsr;
024400080725
024500080731       // ----------------------------------------------------------------------
024600080804       //?Gestione subfile
024700080731       // ----------------------------------------------------------------------
024800080804       begsr gess01;
024900080731
025000080731       //?ho caricato dati emetto il subfile
025100080801         if nrr > 0;
025200080804          exsr sr_sfl01;
025300080731         endif;
025400080731
025500080731       //?non ho caricato dati emetto videata di segnalazione
025600080801         if nrr = 0;
025700080804          exsr sr_d01;
025800080731         endif;
025900080731
026000080731       endsr;
026100080729
026200080731       // ----------------------------------------------------------------------
026300080731       //?Carico i dati per la seconda videata (subfile distinte)
026400080731       // ----------------------------------------------------------------------
026500080729       begsr sr_carica;
026600080730
026700080804       //?pulisco il subfile
026800080804         exsr sr_puls01;
026900080804
027000080804       //?carico la distinta da elaborare
027100080804         exsr sr_cardst;
027200080804
027300080804       //?elaboro i dati della distinta
027400080729         exsr sr_elabdst;
027500080806
027600080806       //?ordino il subfile per tipo
027700080806         exsr sr_ordina;
027800080729
027900080729       endsr;
028000080801
028100080731       // ----------------------------------------------------------------------
028200080731       //?Emetto il subfile distinte
028300080731       // ----------------------------------------------------------------------
028400080804       begsr sr_sfl01;
028500080801
028600121204       //?se ci sono delle righe con errori di trasmissione
028700121204         *in44 = *off;
028800121204         If flg_errdata ='S';
028900121204         *in44 = *on;
029000121204         endif;
029100121204
029200080731       //?emetto la videata
029300080731         write lvp0t01;
029400080804         write lvp0z01;
029500080804         exfmt lvp0c01;
029600080731
029700080731       //?spengo indicatori di comodo
029800080731         *in28 = *off;
029900080731       //?pulisco campo messaggi
030000080804         clear v1cmsg;
030100080731       //?esecuzione comandi
030200080731         select;
030300080731
030400080731       //?f3=fine
030500080731          when *inkc;
030600080731           exsr sr_fine;
030700110622       //?f11=ordina per cod. mancata consegna
030800110622          when *inkk and *in11;
030900110622           *in11 = *off;
031000110622           *in12 = *on;
031100110623           exsr sr_ordina;
031200110622       //?f11=ordina per ora evento
031300110622          when *inkk and *in12;
031400110622           *in12 = *off;
031501140213           *in11 = *on;
031600110622           exsr sr_ordina;
031700080731       //?f12=ritorno
031800080731          when *inkl;
031900080804           exsr sr_fine;
032000080731         other;
032100080805           exsr sr_ctrs01;
032200080731         endsl;
032300080731
032400080731       endsr;
032500080731
032600080731       // ----------------------------------------------------------------------
032700080804       //?Emetto video di segnalazione dati non caricati
032800080731       // ----------------------------------------------------------------------
032900080804       begsr sr_d01;
033000080731
033100080731       //?emetto la videata
033200080731         write lvp0t01;
033300080804         exfmt lvp0d01;
033400080731
033500080731       //?spengo indicatori di comodo
033600080731         *in28 = *off;
033700080731       //?pulisco campo messaggi
033800080804         clear v1cmsg;
033900080731       //?esecuzione comandi
034000080731         select;
034100080731
034200080731       //?f3=fine
034300080731          when *inkc;
034400080731           exsr sr_fine;
034500080731       //?f12=ritorno
034600080731          when *inkl;
034700080804           exsr sr_fine;
034800080813          other;
034900080731         endsl;
035000080731
035100080731       endsr;
035200080730
035300080731       // ----------------------------------------------------------------------
035400080804       //?Pulisco subfile
035500080731       // ----------------------------------------------------------------------
035600080804       begsr sr_puls01;
035700080730
035800080801         clear nrr;
035900080801
036000080730         *in20 = *off;
036100080730         *in21 = *off;
036200080804         write lvp0c01;
036300080730         *in20 = *on;
036400080730         *in21 = *on;
036500080730
036600080804         recsf1 = 1;
036700080730         *in31 = *off;
036800121204         flg_errdata = *blank;
036900080730
037000080730       endsr;
037100080729
037200080731       // ----------------------------------------------------------------------
037300080804       //?Carico la distinta
037400080731       // ----------------------------------------------------------------------
037500080729       begsr sr_cardst;
037600080729
037700110622       //?imposto i dati relativi alla testata distinta
037800110622          exec sql
037900110623           select dstfgs, dstpdr, dstdfv, dstnfv, dstpda, apdrsc
038000110623           into :dstfgs, :dstpdr, :dstdfv, :dstnfv, :dstpda, :apdrsc
038100110623           from fidst01l join
038200110623           fiapd00f on apdpdr = dstpdr and apdtip = 'A'
038300110622           where dstnpg = 4 and dstnfv = :lvp0ndc and
038400110629           dstfgs = :lvp0fgs and dstpdr = :lvp0aut;
038500110622       //?non trovo il rcd corrispondente vado a fine
038600110622          if sqlcod <> 0;
038700110622           leavesr;
038800110622          endif;
038900110622
039000110622          v1cfgs = dstfgs;
039100110622          v1cndc = dstnfv;
039200110622
039300110622       //?data distinta
039400110622         clear wlbdat;
039500110622         g02inv = dstdfv;
039600110622         g02err = '3';
039700110622         xsrda8(wlbdat);
039800110622         v1cddc = g02dat;
039900110622
040000110622         v1caut = lvp0aut;
040100110622         v1cdaut = apdrsc;
040200110622         endsr;
040300080729
040400080731       // ----------------------------------------------------------------------
040500080804       //?Elaboro la distinta
040600080731       // ----------------------------------------------------------------------
040700080730       begsr sr_elabdst;
040800080806
040900080806         $finearb = *off;
041000080806         $fineorg = *off;
041100080729
041200080804       //?cerco cosa è in distinta come consegne
041300080806         if dstpda = 'C' or dstpda = 'E';
041400080806          exsr sr_consegne;
041500080806         endif;
041600080805
041700080804       //?cerco cosa è in distinta come ritiri
041800080806         if dstpda = 'O' or dstpda = 'E';
041900080806          exsr sr_ritiri;
042000110705       //?verifico se ci sono orm manuali
042100110705          exsr sr_ritman;
042200080806         endif;
042300080801
042400080801       //?indicatore per fine file
042500080801         *in31 = *on;
042600080805
042700080801       //?imposto il numero di record caricati
042800080801         rrnlast = nrr;
042900080729
043000080729       endsr;
043100080805
043200101122       // ----------------------------------------------------------------------
043300101122       //?Controllo opzioni immesse sul subfile
043400101122       // ----------------------------------------------------------------------
043500101122       begsr sr_ctrs01;
043600101122
043700080805       //?controllo l'opzione immessa
043800080805         for xx = 1 to rrnlast;
043900080805          chain xx lvp0s01;
044000080806          *in40 = *off;
044100080806       //?visualizza
044200110622          if v1sopz = '5';
044300080806           recsf1 = xx;
044400080806       //?spedizione
044500080806           if v1stipo = 'C';
044600080806            exsr sr_visbolla;
044700080806           endif;
044800080806       //?ritiro
044900110707           if v1stipo = 'R' and v1hman = ' ';
045000080806            exsr sr_visorm;
045100080806           endif;
045200110623          endif;
045201131126       //?visualizza
045202131127          if v1sopz = '7';
045203131126           recsf1 = xx;
045204131126            exsr sr_esiti;
045205131126          endif;
045300080806           clear  v1sopz;
045400080806          update lvp0s01;
045500080805         endfor;
045600080805
045700080805       endsr;
045800080805
045900080805       // ----------------------------------------------------------------------
046000080805       //?Cerco dati consegne in distinta
046100080805       // ----------------------------------------------------------------------
046200080805       begsr sr_consegne;
046300080805
046400110629       //?estraggo prima ARB e PLG delle distinta scelta (WITH)
046500110629       //?poi leggo ARB e dove presente
046600110629       //?(left outer join), aggancio il file dei log per distinta
046700110629       //?e spedizione, per prendere il CET con data ricez. maggiore
046800080805         exec sql
046900110629          declare arb cursor for
047000110629
047100110629          with selARB as (select fnarb00f.*
047200110629          from fnarb00f WHERE arbifp=:dstfgs and
047300110629          arbndc=:dstnfv and arbpdc=:dstpdr),
047400110629
047500110629          selPLG as (select fiplg00f.*
047600110629          from fiplg00f WHERE plgfgs=:dstfgs and
047700110629          plgndc=:dstnfv and plgcodaut=:dstpdr and plgtiporcd = 'CET' and
047800110707          PLGTIPAPPL = 'C' and plgerr = ' ' )
047900110629
048000110629          select ARBAAS, ARBLNP, ARBNRS, ARBNSP, ARBRSD, arbind,
048100140103          ARBlod, ARBPKB, arbvlb, arbtsp, arbicc, arbica,
048200140103          maxora, ifnull(plgdati, ' '),  ifnull(plgMSG, ' ')
048300121128
048400121128          from (SELECT ARBAAS, ARBLNP, ARBNRS, ARBNSP, ARBRSD, arbind,
048500140103          ARBLOD, ARBPKB, arbvlb, arbtsp, arbicc, arbica,
048600121128          ifnull(max(plgdatora), ' ') maxora
048700110629          FROM selarb left outer join selplg on
048800110622          arbndc = plgndc and
048900110622          arbifp = plgfgs and
049000110622          arbpdc = plgcodaut and
049100110622          digits(arbaas)= substr(plgiddoc, 1, 4) and
049200110622          digits(arblnp)= substr(plgiddoc, 5, 3) and
049300110622          digits(arbnrs)= substr(plgiddoc, 8, 2) and
049400110622          digits(arbnsp)= substr(plgiddoc, 10, 7) and
049500110629          plgtiporcd = 'CET' and
049600110629          plgtipappl = 'C'
049700110623          WHERE arbifp=:dstfgs and arbndc=:dstnfv and arbpdc=:dstpdr
049800110622          group by
049900110623          ARBAAS, ARBLNP, ARBNRS, ARBNSP, ARBRSD, ARBIND, ARBLOD,
050000140103          ARBPKB, ARBVLB, arbtsp, arbicc, arbica) as pippo
050100110629
050200110629          left outer join selplg  on
050300110622          digits(arbaas)= substr(plgiddoc, 1, 4) and
050400110622          digits(arblnp)= substr(plgiddoc, 5, 3) and
050500110622          digits(arbnrs)= substr(plgiddoc, 8, 2) and
050600110622          digits(arbnsp)= substr(plgiddoc, 10, 7) and
050700110622          plgtiporcd = 'CET' and
050800110629          plgtipappl = 'C' and
050900110629          maxora = plgdatora;
051000110623
051100080805         exec sql open arb;
051200110623
051300080806         dow not $finearb;
051400110622          exec sql fetch next from arb into
051500110622          :ARBAAS, :ARBLNP, :ARBNRS, :ARBNSP, :ARBRSD, :arbind,
051600140103          :ARBlod, :ARBPKB, :arbvlb, :arbtsp, :arbicc, :arbica,
051700121128          :plgdatora, :plgdati, :plgMSG ;
051800080805       //?fine file o errore sql esco
051900080805          if sqlcod = 100 or sqlcod < 0;
052000080806           $finearb = *on;
052100080806           leave;
052200080805          endif;
052300080805
052400080805       //?imposto i dati della spedizione
052500080805          v1stipo = 'C';
052600110707          v1hman = ' ';
052700110707          v1saas = arbaas;
052800080805          v1slnp = arblnp;
052900080805          v1snrs = arbnrs;
053000080805          v1snsp = arbnsp;
053100080805          v1srag = arbrsd;
053200080805          v1sind = arbind;
053300080805          v1sloc = arblod;
053400080805          v1spkg = arbpkb;
053500121120          v1stsp = '  ';
053600121120       //?il campo è diventato di 2 bytes x poter gestire a video
053700121120       //?un asterisco di errata ricezione su PDA dei dati.
053800120528          if arbtsp = 'E' or arbtsp = 'H';
053900121120            %subst(v1stsp:1:1) = arbtsp;
054000120528          endif;
054100080805          v1svlm = arbvlb;
054200110623          v1spoe = 0;
054300110623          v1snsr = 0;
054400110623          v1snor = 0;
054500110623          v1snrv = 0;
054600080806
054700080806       //?pulisco i dati che possono essere anche a blank
054800110622         clear v1sora;
054900110624         clear v1dcmc;
055000110622         clear v1scmc;
055001140103         clear wincas;
055003140206         clear v1soras;
055004140206         clear v1sord;
055005140211      /end-free
055006140211      *ricerca ora stimata
055007140211     c                   eval      v1hmisto = '00000000' +  'C' + '000000'
055008140211     c                   clear                   v1soras
055009140211     c                   clear                   v1sord
055010140211     c     kpctcor       chain     fipct02l
055011140211     c                   if        %found(fipct02l)
055012140211     c                   movel     pctdati       fipctcords
055013140211     c                   eval      v1hmisto = %editc(§PCTORASTI:'X') +
055014140211     c                             %editc(§PCTSORT:'X') +
055015140211     c                             'C' + '000000'
055016140211     c                   if        §pctorasti > 0
055017140211     c                   eval      v1soras = §PCTORASTI
055018140211     c                   endif
055019140211     c                   if        §pctsort  > 0
055020140211     c                   eval      v1sord = §PCTsort
055021140211     c                   endif
055022140211     c                   endif
055023140211      /free
055100110623         if plgdati <> ' ';
055200121102
055300110622          eval fiprdcetds = plgdati;
055400121102
055500121102      /end-free
055600121102      *
055700121102     c                   if          plgMSG = Error_DATA and plgDATora<>' '
055800121120     c**  Al momento SI EVIDENZIA con un ASTERISCO la ricezione anomala
055900121120     c                   eval      %subst(v1stsp:2:1) = '*'
056000121204     c                   eval      flg_errdata = 'S'
056100121120      ***  se presente una data/ora non corretta
056200121120      ***  prende la data ora (senza i secondi) della ricezione
056300121120      ***  e la sostituisce.
056400121120     c*********          eval      wrkDAThhmm = %subst(plgDATora: 1: 12)
056500121120     c*********          movel     wrkDAThhmm    §CETDTORIN
056600121120     c*********          move      §CETDTORIN    §CETora
056700121102     c                   end
056701140103      * verifica incassi
056702140103     c                   clear                   wincas            3
056703140103     c                   if        arbicc = 'R'  and
056704140103     c                             (§CETINCCAS = 'N' or
056705140103     c                              §CETINCCAS = ' ')
056706140103     c                             OR
056707140103     c                             arbica = 'R'  and
056708140103     c                             (§CETINCFAT = 'N' or
056709140103     c                             §CETINCFAT = ' ')
056710140103     c                   move      '  N'         wincas
056711140103     c                   endif
056730140124      *rileva presenza doppio evento rekor cep
056731131126     c     kpct          setll     fipct02l                               66
056732131126     c   66              move      '*'           v1scep
056733131126     c  n66              clear                   v1scep
056800121102      /free
056900121102
057000110623          eval v1scmc = §cetcmc;
057100110629          v1sora = §cetora ;
057200110624          if v1scmc = ' ';
057300110623          v1scmc = '  C';
057400110624          v1dcmc = 'CONSEGNATA';
057401140103           if wincas = '  N';
057402140103            v1scmc = '  N';
057403140103            v1dcmc = 'NON FATTA ';
057404140103           endif;
057500140103          else;
057600110624                 tblKEy = v1scmc;
057700110624                 chain (tblkut:tblcod:tblkey) tabel00f;
057800110624                 clear v1dcmc;
057900110624                   if  %found(tabel00f);
058000110624                      ds2a = tblUNI;
058100110624                      v1dcmc= §2ades;
058200110624                   endif;
058300110622          endif;
058400110622         endif;
058500080805
058600080805       //?scrivo subfile
058700080805          exsr sr_cars01;
058800080805
058900080805         enddo;
059000080806
059100080806       //?chiudo il cursore
059200080806          exec sql close arb;
059300080805
059400080805       endsr;
059500080813
059600080805       // ----------------------------------------------------------------------
059700080805       //?Cerco dati ritiri in distinta
059800080805       // ----------------------------------------------------------------------
059900080805       begsr sr_ritiri;
060000080805
060100110629       //?estraggo prima i ritiri in distinta su ORM e PLG (WITH)
060200110629       //?poi leggo i ritiri in distinta su ORM e dove presente
060300110629       //?(left outer join) aggancio il file dei log per distinta
060400110629       //?e orm, per prendere il RES con data ricez. maggiore
060500080805         exec sql
060600110629          declare org cursor for
060700110629
060800110629          with selorg as (select fnorg00f.*
060900110629          from fnorg00f WHERE orgfgs=:dstfgs and
061000110629          orgndc=:dstnfv and orgpdc=:dstpdr),
061100110629
061200110629          selorm as (select fnorm00f.*
061300110629          from fnorm00f WHERE ormpor=:dstfgs and
061400110629          ormndc=:dstnfv),
061500110629
061600110629          selPLG as (select fiplg00f.*
061700110629          from fiplg00f WHERE plgfgs=:dstfgs and
061800110629          plgndc=:dstnfv and plgcodaut=:dstpdr and plgtiporcd = 'RES' and
061900110707          PLGTIPAPPL = 'R' and plgerr = ' ')
062000110629
062100121128          SELECT
062200121128          orgpoe, orgnsr, orgnor, orgnrv, ormrsr, orminr, ormlor, orgpkg,
062300121128          orgvlm, maxora, ifnull(plgdati, ' '), ifnull(plgmsg, ' ')
062400121128          FROM
062500121128
062600121128           (select
062700121128          orgpoe, orgnsr, orgnor, orgnrv, ormrsr, orminr, ormlor, orgpkg,
062800121128          orgvlm, ifnull(max(plgdatora), ' ') maxora
062900121128          FROM selorg
063000121128          JOIN selorm
063100121128            ON
063200110629          orgpoe= ormpoe and
063300110629          orgnsr= ormnsr and
063400110629          orgnor= ormnor and
063500110629          orgnrv= ormnrv
063600121128          LEFT outer
063700121128          JOIN selplg
063800121128            ON
063900110622          orgndc = plgndc and
064000110622          orgfgs = plgfgs and
064100110622          orgpdc = plgcodaut and
064200110622          digits(orgpoe)= substr(plgiddoc, 1, 3) and
064300110622          digits(orgnsr)= substr(plgiddoc, 4, 2) and
064400110623          digits(orgnor)= substr(plgiddoc, 6, 7) and
064500110623          digits(orgnrv)= substr(plgiddoc, 13, 2) and
064600110622          plgtiporcd = 'RES'
064700121128
064800121128          WHERE
064900121128          orgfgs=:dstfgs and orgndc=:dstnfv and orgpdc=:dstpdr
065000121128          GROUP BY
065100121128          orgpoe, orgnsr, orgnor, orgnrv, ormrsr, orminr, ormlor, orgpkg,
065200121128          orgvlm ) AS PIPPO
065300110623
065400121128          LEFT outer
065500121128          JOIN selplg
065600121128            ON
065700110622          digits(orgpoe)= substr(plgiddoc, 1, 3) and
065800110622          digits(orgnsr)= substr(plgiddoc, 4, 2) and
065900110623          digits(orgnor)= substr(plgiddoc, 6, 7) and
066000110623          digits(orgnrv)= substr(plgiddoc, 13, 2) and
066100110622          plgtiporcd = 'RES' and
066200110629          maxora = plgdatora;
066300110623
066400110622         exec sql open org;
066500110623
066600110622         dow not $fineorg;
066700110622          exec sql fetch next from org into
066800121128          :orgpoe, :orgnsr, :orgnor, :orgnrv, :ormrsr, :orminr,
066900121128          :ormlor, :orgpkg, :orgvlm, :plgdatora, :plgdati, :plgmsg ;
067000110623
067100080805       //?fine file o errore sql esco
067200080805          if sqlcod = 100 or sqlcod < 0;
067300080806           $fineorg = *on;
067400080805           leave;
067500080805          endif;
067600080805
067700080805       //?imposto i dati del ritiro
067800080805          v1stipo = 'R';
067900080805          v1spoe = orgpoe;
068000080805          v1snsr = orgnsr;
068100080805          v1snor = orgnor;
068200080805          v1snrv = orgnrv;
068300080805          v1srag = ormrsr;
068400080805          v1sind = orminr;
068500080805          v1sloc = ormlor;
068600080915          v1spkg = orgpkg;
068700080915          v1svlm = orgvlm;
068800121120          v1stsp = '  ';
068900110707          v1hman = ' ';
069000110623          v1saas = 0;
069100110623          v1slnp = 0;
069200110623          v1snrs = 0;
069300110623          v1snsp = 0;
069400080806
069500110622       //?pulisco i dati che possono essere anche a blank
069600110622         clear v1sora;
069700110622         clear v1scmc;
069800110624         clear v1dcmc;
069801131211         clear v1scep;
069802140206         v1hmisto = '00000000' +  'R' + '000000' ;
069803140124         clear v1soras;
069804140124         clear v1sord;
069805140213       /end-free
069807140213      *ricerca ora stimata
069808140213     c                   eval      v1hmisto = '00000000' +  'R' + '000000'
069809140213     c                   clear                   v1soras
069810140213     c                   clear                   v1sord
069811140213     c     kproror       chain     fipro11l
069812140213     c                   if        %found(fipro11l)
069813140213     c                   movel     prodati       fiproRORds
069814140213     c                   eval      v1hmisto = %editc(§proORASTI:'X') +
069815140213     c                             %editc(§ProSORT:'X') +
069816140213     c                             'R' + '000000'
069817140213     c                   if        §proorasti > 0
069818140213     c                   eval      v1soras = §ProORASTI
069819140213     c                   endif
069820140213     c                   if        §prosort  > 0
069821140213     c                   eval      v1sord = §Prosort
069822140213     c                   endif
069823140213     c                   endif
069824140213      /free
069900110623         if plgdati <> ' ' ;
070000121102
070100110623          eval FIPRCRESDS = plgdati;
070200121102
070300121102      /end-free
070400121102      *
070500121102     c                   if          plgMSG = Error_DATA and plgDATora<>' '
070600121120     c**  Al momento SI EVIDENZIA con un ASTERISCO la ricezione anomala
070700121120     c                   eval      %subst(v1stsp:2:1) = '*'
070800121204     c                   eval      flg_errdata = 'S'
070900121120      ***  se presente una data/ora non corretta
071000121120      ***  prende la data ora (senza i secondi) della ricezione
071100121120      ***  e la sostituisce.
071200121120     c**************     eval      wrkDAThhmm = %subst(plgDATora: 1: 12)
071300121120     c**************     movel     wrkDAThhmm    §RESDATORA
071400121120     c**************     movel     wrkDAThhmm    §RESDTORIN
071500121102     c                   end
071501140124      *ricerca ora stimata per ora inchioda costante
071502140124     c                   eval      v1hmisto = '00000000' +  'R' + '000000'
071600121102      /free
071700121102
071800110623          eval v1scmc = §rescmr;
071900110629           ora = %subst(§RESDATORA: 9: 6);
072000110629           v1sora = %dec(ora:6:0);
072100110622          if v1scmc = ' ';
072200110623          v1scmc = '  R';
072300110624          v1dcmc = 'RITIRATO';
072400110624          else;
072500110624                 tbeke1 = v1scmc;
072600110624                 chain (tbecod:tbeke1) tntbe01l;
072700110624                 clear v1dcmc;
072800110624                   if  %found(tntbe01l);
072900110624                      dcmr = tbeUNI;
073000110624                      v1dcmc= D§CMRDES;
073100110707                   else;
073200110707                   iter;
073300110624                   endif;
073400110622          endif;
073500110622         endif;
073600080805
073700080805       //?scrivo subfile
073800080805          exsr sr_cars01;
073900080805
074000080805         enddo;
074100080806
074200080806       //?chiudo il cursore
074300080813         exec sql close org;
074400080805
074500080805       endsr;
074600080813
074700110705       // ----------------------------------------------------------------------
074800110705       //?Cerco dati ritiri manuali
074900110705       // ----------------------------------------------------------------------
075000110705       begsr sr_ritman;
075100110705
075200110705       //?estraggo i ritiri inseriti manualmente
075300110705         exec sql
075400110705          declare orgm cursor for
075500110705
075600110705          with
075700110705          selPLG as (select plgiddoc, max(plgdatora) max
075800110705          from fiplg00f WHERE plgfgs=:dstfgs and
075900110705          plgndc=:dstnfv and plgcodaut=:dstpdr and plgtiporcd = 'RES' and
076000110707          PLGTIPAPPL = 'R' and substr(plgiddoc, 1, 8)='00000000'  and
076100110707          PLGerr = ' '
076200110705          group by plgiddoc)
076300110705
076400110705          select fiplg00f.*  from selPLG join fiplg00f on
076500110705          selplg.plgiddoc = fiplg00f.plgiddoc and
076600110705          max = fiplg00f.plgdatora ;
076700110705
076800110705         exec sql open orgm;
076900110705
077000110705         dow not $fineorgm;
077100110705          exec sql fetch next from orgm into
077200110705          :fiplgds;
077300110705
077400110705       //?fine file o errore sql esco
077500110705          if sqlcod = 100 or sqlcod < 0;
077600110705           $fineorgm = *on;
077700110705           leave;
077800110705          endif;
077900110705
078000110705       //?imposto i dati del ritiro
078100121102
078200110705          eval FIPRCRESDS = plgdati;
078300121102
078400121102      /end-free
078500121102      *
078600121120     c                   clear                   v1stsp
078700121102     c                   if          plgMSG = Error_DATA and plgDATora<>' '
078800121120     c**  Al momento SI EVIDENZIA con un ASTERISCO la ricezione anomala
078900121120     c                   eval      %subst(v1stsp:2:1) = '*'
079000121204     c                   eval      flg_errdata = 'S'
079100121120      ***  se presente una data/ora non corretta
079200121120      ***  prende la data ora (senza i secondi) della ricezione
079300121120      ***  e la sostituisce.
079400121120     c*************      eval      wrkDAThhmm = %subst(plgDATora: 1: 12)
079500121120     c*************      movel     wrkDAThhmm    §RESDATORA
079600121120     c*************      movel     wrkDAThhmm    §RESDTORIN
079700121102     c                   end
079800121102      /free
079900121102
080000110705          v1stipo = 'R';
080100110705          v1spoe = %dec(§RESFGS:3:0);
080200110705          v1snsr = %dec(§RESNSR:6:0);
080300110705          v1snor = %dec(§RESNOR:7:0);
080400110705          v1snrv = %dec(§RESNRV:2:0);
080500110705          v1srag = §RESRSR;
080600110705          v1sind = §RESINR;
080700110705          v1sloc = ' ';
080800121120       // v1stsp = ' ';     pulito il campo e caricato poco sopra
080900110705          v1spkg = 0;
081000110705          v1svlm = 0;
081100110707          v1hman = 'M';
081200110705          v1saas = 0;
081300110705          v1slnp = 0;
081400110705          v1snrs = 0;
081500110705          v1snsp = 0;
081600110705
081700110705       //?pulisco i dati che possono essere anche a blank
081800110705         clear v1sora;
081900110705         eval v1scmc = §rescmr;
082000110705         ora = %subst(§RESDATORA: 9: 6);
082100110705         v1sora = %dec(ora:6:0);
082200110705          if v1scmc = ' ';
082300110705           v1scmc = '  R';
082400110705           v1dcmc = 'RITIRATO';
082500110705          else;
082600110705           tbeke1 = v1scmc;
082700110705           chain (tbecod:tbeke1) tntbe01l;
082800110705           clear v1dcmc;
082900110705           if  %found(tntbe01l);
083000110705            dcmr = tbeUNI;
083100110705            v1dcmc= D§CMRDES;
083200110707           else;
083300110707           iter;
083400110705           endif;
083500110705          endif;
083600110705
083700110705       //?scrivo subfile
083800110705          exsr sr_cars01;
083900110705
084000110705         enddo;
084100110705
084200110705       //?chiudo il cursore
084300110705         exec sql close orgm;
084400110705
084500110705       endsr;
084600110705
084700080801       // ----------------------------------------------------------------------
084800080805       //?Carico il subfile
084900080801       // ----------------------------------------------------------------------
085000080805       begsr sr_cars01;
085100080805
085200080805       //?carico solo se ha un'anomalia
085300080805          clear v1sopz;
085400080801          nrr = nrr + 1;
085500080805          write lvp0s01;
085501131211          clear v1scep;
085502140124          clear v1soras;
085503140124          clear v1sord;
085600080801
085700080801       endsr;
085800080806
085900080806       // ----------------------------------------------------------------------
086000080806       //?Visualizzo la spedizione
086100080806       // ----------------------------------------------------------------------
086200080806       begsr sr_visbolla;
086300080806
086400080806         clear p36ds;
086500080806         p36aas = v1saas;
086600080806         p36lnp = v1slnp;
086700080806         p36nrs = v1snrs;
086800080806         p36nsp = v1snsp;
086900080806         p36flg = '1';
087000080806         clear p36f03;
087100080806         kpjbu = p36ds;
087200080806         fnlr36r(kpjba);
087300080806
087400080806       endsr;
087500080806
087600080806       // ----------------------------------------------------------------------
087601131126       // ----------------------------------------------------------------------
087602131126       //?Visualizzo la spedizione
087603131126       // ----------------------------------------------------------------------
087604131126       begsr sr_esiti;
087605131126
087606131126         clear fidg46ds ;
087607131126         DG46FGSI  = v1cfgs;
087608131126         DG46NDCI  = v1cndc;
087609131126         DG46CODAUT= v1caut;
087610131126         DG46AASI  = v1saas;
087611131126         DG46LNPI  = v1slnp;
087612131126         DG46NRSI  = v1snrs;
087613131126         DG46NSPI  = v1snsp;
087614131126
087615131126
087616131126
087617131126         kpjbu = fidg46ds ;
087618131126         fidg49r(kpjba);
087619131126
087620131126       endsr;
087621131126
087622131126       // ----------------------------------------------------------------------
087700080806       //?Visualizzo l'ORM
087800080806       // ----------------------------------------------------------------------
087900080806       begsr sr_visorm;
088000080806
088100080806         clear p07ds;
088200080806         p07poe = v1spoe;
088300080806         p07nor = v1snor;
088400080806         p07nsr = v1snsr;
088500080806         p07nrv = v1snrv;
088600080806         p07sce = '5';
088700080806         p07fgs = v1cfgs;
088800080806         kpjbu = p07ds;
088900080806         fior07r(kpjba);
089000080806
089100080806       endsr;
089200110217
089300080801       // ----------------------------------------------------------------------
089400080804       //?Ordino il subfile prima di emetterlo
089500080801       // ----------------------------------------------------------------------
089600080804       begsr sr_ordina;
089700080801
089800080804        recsf1 = 1;
089900080801
090000080801       // inizializza i campi chiave x l'ordinamento. C'è un campo in più non
090100080801       // presente nel subfile -- "Selected"?-- questo è aggiunto al record.
090200080801       // il campo è usato per selezionare i records dando un ordine a quelli
090300080801       // selezionati davanti ai non selezionati.
090400080801         clear QLGSCB;
090500080801         clear QLGSCB00;
090600080801
090900080801
091000110623       // imposto la posizione dei campi chiave sul subfile e la lunghezza
091100080801       // l'ordinamento è su un campo numerico e deve essere
091200110623       // descending
091300110623       if *in11;
091301140213       // campi chiave
091302140213         QLGNBRK = 2;
091400110623         QLGSP=1+%size(v1stipo)+
091500110623                 %size(v1srag)+%size(v1sind)+%size(v1sloc)+%size(v1spkg)+
091600110623                 %size(v1svlm)+%size(v1scmc);
091700140213         QLGSS = %SIZE(v1sora);
091800110623         QLGDT = numerico;
091801140213         QLGSO = discendente;
091802140213         QLGKL(1) = QLGSKL;
091803140213         QLGSP=1+%size(v1stipo)+
091804140213                 %size(v1srag)+%size(v1sind)+%size(v1sloc)+%size(v1spkg)+
091805140213                 %size(v1svlm)+%size(v1scmc)+%size(v1sora);
091806140213         QLGSS = %SIZE(v1hmisto);
091807140213         QLGDT = Carattere;
091808140213         QLGSO = ascendente;
091809140213         QLGKL(2) = QLGSKL;
091900110623       endif;
092000110623       if *in12;
092001140213       // campi chiave
092002140218         QLGNBRK = 3;
092100110623         QLGSP=1+%size(v1stipo)+
092200110623                 %size(v1srag)+%size(v1sind)+%size(v1sloc)+%size(v1spkg)+
092300110623                 %size(v1svlm);
092400110623         QLGSS = %SIZE(v1scmc);
092500080805         QLGDT = Carattere;
092501140218         QLGSO = discendente;
092502140213         QLGKL(1) = QLGSKL;
092503140218         QLGSP=1+%size(v1stipo)+
092504140218                 %size(v1srag)+%size(v1sind)+%size(v1sloc)+%size(v1spkg)+
092505140218                 %size(v1svlm)+%size(v1scmc);
092506140218         QLGSS = %SIZE(v1sora);
092507140218         QLGDT = numerico;
092508140218         QLGSO = discendente;
092509140218         QLGKL(2) = QLGSKL;
092510140218         QLGSP=1+%size(v1stipo)+
092511140218                 %size(v1srag)+%size(v1sind)+%size(v1sloc)+%size(v1spkg)+
092512140218                 %size(v1svlm)+%size(v1scmc)+%size(v1sora);
092513140218         QLGSS = %SIZE(v1hmisto);
092514140218         QLGDT = Carattere;
092515140218         QLGSO = ascendente;
092516140218         QLGKL(3) = QLGSKL;
092600110623       endif;
092900080801
093000080801       // Load other sort parameters.
093100080801         QLGLB = 80 + 16 * MaxKey;
093200080801         QLGRL = %SIZE(sflrcd) - 1;
093300080801         QLGRT = 8;
093400080801         QLGOKL = 80;
093500080801         QLGLKE = 16;
093600080801         QLGLSS = 290;
093700080801
093800080801       // Initialize Sort I/O API fields.
093900080801         QLGRL00 = QLGRL;
094000080801         QLGRC00 = 1;
094100080801         clear QUSEI;
094200080801         QUSBPRV = %SIZE(QUSEC);
094300080801
094400080801       // First step - Initialize the sort routine.
094500080801         QLGSORT_pr(Qlgscb:NotUsed:NotUsed:SizeList:ReturnSize:Qusec);
094600080801
094700080801       // Next step - Write records to I/O routine.
094800080801         QLGRT00 = Put;
094900080801         for xx = 1 to rrnlast;
095000080805          chain xx lvp0s01;
095100080801
095200080801       // solo le righe con Selected = 'Y' sono riordinate,
095300080801       // quindi per fare un ordinamento di tutte le righe
095400080801       // metto 'Y' sempre.
095500080801          selected  = 'Y';
095600080801          clear QUSEI;
095700080801          QUSBPRV = %SIZE(QUSEC);
095800080801          QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
095900080801         endfor;
096000080801
096100080801       // Next step - Signal end of input, clear subfile for reload.
096200080801         QLGRT00 = EndPut;
096300080801         clear QUSEI;
096400080801         QUSBPRV = %SIZE(QUSEC);
096500080801         QLGSRTIO_pr(Qlgscb00:SflRcd:NotUsed:SizeList:NotUsed:Qusec);
096600080801       // pulizia SFL
096700080801         clear nrr;
096800080801         *in20 = *off;
096900080801         *in21 = *off;
097000080805         write lvp0c01;
097100080801         *in20 = *on;
097200080801         *in21 = *on;
097300080801
097400080801       // Final step - Write the records back to the subfile.
097500080801         QLGRT00 = Get;
097600080801         for xx = 1 to rrnlast;
097700080801          clear QUSEI;
097800080801          QUSBPRV = %SIZE(QUSEC);
097900080801          QLGSRTIO_pr2(Qlgscb00:NotUsed:SflRcd:Qlgrl00:NotUsed:Qusec);
098000080801          nrr = xx;
098100080805          write lvp0s01;
098200080801         endfor;
098300080801
098400080801       endsr;
098500080725
098600080731       // ----------------------------------------------------------------------
098700080731       //?Fine programma
098800080731       // ----------------------------------------------------------------------
098900080725       begsr sr_fine;
099000080725
099100080725         *inlr = *on;
099200080725         return;
099300080725
099400080725       endsr;
099500070214
099600080805** MSG  Lungh. 78                                                            *
099700080806Opzione non valida                                                            01
099800101124Solo x le consegne                                                            02
099900101126Non ci sono NOTE                                                              03
100000101201Opzione non Attiva                                                            04
100100110217Errori durante la fase di variazione Bolle                                    05
